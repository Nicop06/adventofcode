import Data.List (nub)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

data RobotType = OreRobot | ClayRobot | ObsidianRobot | GeodeRobot deriving (Show, Eq, Ord, Bounded, Enum)

data Resources = Resources {getOre :: Int, getClay :: Int, getObsidian :: Int, getGeode :: Int} deriving (Show, Eq)

data Blueprint = Blueprint {blueprintId :: Int, oreRobotCost :: Resources, clayRobotCost :: Resources, obsidianRobotCost :: Resources, geodeRobotCost :: Resources} deriving (Show, Eq)

data SimState = SimState {stateResources :: Resources, numOreRobot :: Int, numClayRobot :: Int, numObsidianRobot :: Int, numGeodeRobot :: Int, simulationTime :: Int} deriving (Show, Eq)

-- Helpers

canBuild :: Resources -> SimState -> Bool
canBuild cost state =
  let res = stateResources state
   in getOre cost <= getOre res
        && getClay cost <= getClay res
        && getObsidian cost <= getObsidian res

zeroResources :: Resources
zeroResources = Resources 0 0 0 0

initialState :: SimState
initialState =
  SimState
    { stateResources = zeroResources,
      numOreRobot = 1,
      numClayRobot = 0,
      numObsidianRobot = 0,
      numGeodeRobot = 0,
      simulationTime = 0
    }

updateResources :: SimState -> SimState
updateResources state =
  state
    { stateResources = Resources ore' clay' obsidian' geode',
      simulationTime = simulationTime state + 1
    }
  where
    res = stateResources state
    ore' = getOre res + numOreRobot state
    clay' = getClay res + numClayRobot state
    obsidian' = getObsidian res + numObsidianRobot state
    geode' = getGeode res + numGeodeRobot state

spendResources :: Resources -> SimState -> SimState
spendResources cost state =
  state {stateResources = Resources ore' clay' obsidian' geode'}
  where
    res = stateResources state
    ore' = getOre res - getOre cost
    clay' = getClay res - getClay cost
    obsidian' = getObsidian res - getObsidian cost
    geode' = getGeode res - getGeode cost

buildRobot :: RobotType -> SimState -> SimState
buildRobot OreRobot state = state {numOreRobot = numOreRobot state + 1}
buildRobot ClayRobot state = state {numClayRobot = numClayRobot state + 1}
buildRobot ObsidianRobot state = state {numObsidianRobot = numObsidianRobot state + 1}
buildRobot GeodeRobot state = state {numGeodeRobot = numGeodeRobot state + 1}

robotCost :: RobotType -> Blueprint -> Resources
robotCost OreRobot = oreRobotCost
robotCost ClayRobot = clayRobotCost
robotCost ObsidianRobot = obsidianRobotCost
robotCost GeodeRobot = geodeRobotCost

robotResource :: RobotType -> Resources -> Int
robotResource OreRobot = getOre
robotResource ClayRobot = getClay
robotResource ObsidianRobot = getObsidian
robotResource GeodeRobot = getGeode

numRobotOfType :: RobotType -> SimState -> Int
numRobotOfType OreRobot = numOreRobot
numRobotOfType ClayRobot = numClayRobot
numRobotOfType ObsidianRobot = numObsidianRobot
numRobotOfType GeodeRobot = numGeodeRobot

buildRobotOfType :: Blueprint -> RobotType -> SimState -> SimState
buildRobotOfType blueprint robotType =
  spendAndBuildRobot . head . dropWhile (not . canBuild cost) . iterate updateResources
  where
    cost = robotCost robotType blueprint
    spendAndBuildRobot = buildRobot robotType . spendResources cost . updateResources

updateUntilTheEnd :: Int -> SimState -> SimState
updateUntilTheEnd numSteps = head . dropWhile ((< numSteps) . simulationTime) . iterate updateResources

updateSimState :: Int -> Blueprint -> SimState -> [SimState]
updateSimState numSteps blueprint state =
  let robotTypesToBuild = filter canAffordToBuild $ robotTypesToConsider numSteps blueprint state
      statesAfterBuildingRobots = map (flip (buildRobotOfType blueprint) state) robotTypesToBuild
      nonTerminatedStates = filter ((< numSteps) . simulationTime) statesAfterBuildingRobots
   in if null nonTerminatedStates
        then [updateUntilTheEnd numSteps state]
        else nonTerminatedStates
  where
    canAffordToBuild robotType = numStepsToBuild state (robotCost robotType blueprint) <= (numSteps - simulationTime state)

numStepsToBuild :: SimState -> Resources -> Int
numStepsToBuild state cost = maximum $ map numStepsForRes [ClayRobot, OreRobot, ObsidianRobot]
  where
    divOrInf a b
      | a == 0 = 0
      | b == 0 = maxBound
      | otherwise = ceiling (fromIntegral a / fromIntegral b)
    numStepsForRes robotType = (robotResource robotType cost - robotResource robotType (stateResources state)) `divOrInf` numRobotOfType robotType state

robotTypesToConsider :: Int -> Blueprint -> SimState -> [RobotType]
robotTypesToConsider numSteps blueprint state =
  GeodeRobot : filter (not . tooManyRobots) [OreRobot, ClayRobot, ObsidianRobot]
  where
    allRobotCosts = map (`robotCost` blueprint) [minBound ..]
    remainingTime = numSteps - simulationTime state
    maxCostForRes getRes = (* remainingTime) . maximum . map getRes $ allRobotCosts
    resAfterSteps robotType = remainingTime * numRobotOfType robotType state + robotResource robotType (stateResources state)
    tooManyRobots robotType = resAfterSteps robotType >= maxCostForRes (robotResource robotType)

runSimulation :: Int -> Blueprint -> [SimState] -> [SimState]
runSimulation numSteps blueprint [] = []
runSimulation numSteps blueprint states =
  let updatedStates = concatMap (updateSimState numSteps blueprint) states
   in filter hasFinished updatedStates ++ runSimulation numSteps blueprint (filter (not . hasFinished) updatedStates)
  where
    hasFinished = (== numSteps) . simulationTime

maxNumGeode :: Int -> Blueprint -> Int
maxNumGeode numSteps blueprint =
  maximum (map (getGeode . stateResources) $ runSimulation numSteps blueprint [initialState])

qualityLevel :: Int -> Blueprint -> Int
qualityLevel numSteps blueprint =
  blueprintId blueprint * maxNumGeode numSteps blueprint

totalQualityLevel :: Int -> [Blueprint] -> [Int]
totalQualityLevel numSteps = map (qualityLevel numSteps)

-- Parser

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseRobotCost :: String -> Parser Resources
parseRobotCost robotType = string (" Each " ++ robotType ++ " robot costs ") *> parseCost <* char '.'

parseResourceAmount :: Parser (Int, String)
parseResourceAmount = (,) <$> (parseNumber <* char ' ') <*> many1 lower

parseCost :: Parser Resources
parseCost = foldl addResource zeroResources <$> parseResourceAmount `sepBy1` string " and "

addResource :: Resources -> (Int, String) -> Resources
addResource res (amount, "ore") = res {getOre = getOre res + amount}
addResource res (amount, "clay") = res {getClay = getClay res + amount}
addResource res (amount, "obsidian") = res {getObsidian = getObsidian res + amount}
addResource res (amount, resType) = error $ "Unknown resource type " ++ resType

parseBlueprint :: Parser Blueprint
parseBlueprint = Blueprint <$> (string "Blueprint " *> parseNumber <* string ":") <*> parseRobotCost "ore" <*> parseRobotCost "clay" <*> parseRobotCost "obsidian" <*> parseRobotCost "geode" <* newline

parseAllBlueprints :: Parser [Blueprint]
parseAllBlueprints = many1 parseBlueprint <* eof

part1 :: Parser Int
part1 = sum . totalQualityLevel 24 <$> parseAllBlueprints

--part2 :: Parser Int
part2 = product . map (maxNumGeode 32) . take 3 <$> parseAllBlueprints

main :: IO ()
main = parseAndSolve "inputs/day19" part1 part2
--main = parseAndSolve "inputs/day19_easy" part1 part2
