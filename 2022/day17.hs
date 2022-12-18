import qualified Data.Map.Strict as M
import Control.Applicative (ZipList (..), getZipList)
import Data.Bifunctor (first, second)
import Data.Either (fromRight)
import Data.List
import Data.Maybe (fromMaybe)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

data Jet = JetRight | JetLeft deriving (Show)

data Rock = Hole | Rock deriving (Show, Eq, Ord)

type RockShape = [[Rock]]

type Cave = (Int, RockShape)

type Position = (Int, Int)

type FallingRock = (Position, RockShape)

data CaveState = CaveState {nextShapes :: [(Int, RockShape)], nextJets :: [(Int, Jet)], cave :: Cave, fallingRock :: FallingRock, stepsElapsed :: Int} deriving (Show)

type CacheState = (Int, Int, RockShape)

type Cache = M.Map CacheState (Int, Int)

caveWidth = 7 :: Int

initialCave = (-1, [replicate caveWidth Rock])

-- Helpers

initState :: Int -> [RockShape] -> [Jet] -> CaveState
initState numFalling shapes jets =
  CaveState allShapes allJets initialCave firstFallingRock (numFalling - 1)
  where
    firstFallingRock = newFallingRock (head shapes)
    forever = concat . repeat
    allShapes = take numFalling . tail . concat . repeat $ zip [0..] shapes
    allJets = concat . repeat $ zip [0..] jets

newFallingRock :: RockShape -> FallingRock
newFallingRock shape = ((2, -3), shape)

intersectsWithCave :: Cave -> FallingRock -> Bool
intersectsWithCave _ ((_, _), []) = False
intersectsWithCave cave rock@((_, y), _) =
  let (_, shapesToIntersect) = shapesAboveAndBelowCave rock
      (caveRowsToIntersect, _) = splitAt y (snd cave)
      bothRocks x y = (x, y) == (Rock, Rock)
   in any or $ zipWith bothRocks <$> ZipList shapesToIntersect <*> ZipList caveRowsToIntersect

mergeRockWithCave :: Cave -> FallingRock -> Cave
mergeRockWithCave (h, cave) fr@((x, y), shape) =
  let (rocksToAdd, shapeToMerge) = shapesAboveAndBelowCave fr
      (caveRowsToMerge, rs) = splitAt y cave
      mergedRows = getZipList $ zipWith max <$> ZipList caveRowsToMerge <*> ZipList shapeToMerge
   in (h, rocksToAdd ++ mergedRows ++ rs)

shapesAboveAndBelowCave :: FallingRock -> (RockShape, RockShape)
shapesAboveAndBelowCave ((x, y), shape) = splitAt (length shape - y) (padCol y $ map (padRow x) shape)

padRow :: Int -> [Rock] -> [Rock]
padRow offset rocks = take 7 $ rocksLeft ++ rocks ++ repeat Hole
  where
    rocksLeft = replicate offset Hole

padCol :: Int -> RockShape -> RockShape
padCol offset rocks = replicate (offset - length rocks) (replicate caveWidth Hole) ++ rocks

caveRowAt :: Cave -> Int -> [Rock]
caveRowAt cave i =
  let (_, rs) = splitAt i (snd cave)
   in case rs of
        row : _ -> row
        [] -> replicate caveWidth Hole

pushRock :: Jet -> CaveState -> CaveState
pushRock JetLeft = fromMaybe <$> id <*> updateRockPosition (first (subtract 1))
pushRock JetRight = fromMaybe <$> id <*> updateRockPosition (first (+ 1))

updateRockPosition :: (Position -> Position) -> CaveState -> Maybe CaveState
updateRockPosition f state@(CaveState shapes jets cave r@(p, rs) _) =
  let (x, y) = f p
      newRock = ((x, y), rs)
   in if x < 0 || x + (length . head $ rs) > caveWidth || intersectsWithCave cave newRock
        then Nothing
        else Just $ state {fallingRock = newRock}

rockFalls :: Cache -> CaveState -> (Cache, CaveState)
rockFalls cache state@(CaveState shapes jets cave rock steps) =
  case updateRockPosition (second (+ 1)) state of
    Just newState -> (cache, newState)
    Nothing ->
      let newState = state
            { cave = updateCaveHeight $ mergeRockWithCave cave rock,
              fallingRock = newFallingRock (snd $ head shapes),
              nextShapes = tail shapes,
              stepsElapsed = steps - 1
            } in getOrUpdateCache cache newState

getOrUpdateCache :: Cache -> CaveState -> (Cache, CaveState)
getOrUpdateCache cache state@(CaveState shapes jets cave _ steps) =
    let cacheState = (fst . head $ shapes, fst . head $ jets, snd cave) in
        case M.lookup cacheState cache of
            Nothing -> (M.insert cacheState (caveRockHeight cave, steps) cache, state)
            Just (height, steps) -> (cache, reduceState state height steps)

reduceState :: CaveState -> Int -> Int -> CaveState
reduceState (CaveState shapes jets (height, cave) rock steps) cachedHeight cachedSteps =
    let numElem = fst . last . takeWhile ((/=0) . fst)
        numShapes = numElem shapes
        numJets = numElem jets
        stepsDiff = steps - cachedSteps
        remainingSteps = steps `rem` stepsDiff
        numIterations = steps `div` stepsDiff
        heightDiff = height - cachedHeight
        totalHeight = height + numIterations * heightDiff
        newShapes = skipElems shapes numShapes numIterations
        newJets = skipElems jets numJets numIterations
    in CaveState newShapes newJets (totalHeight, cave) rock  steps

skipElems :: [(Int, a)] -> Int -> Int -> [(Int, a)]
skipElems l numElems numIterations =
    let curIdx = fst . head $ l
        newIdx = (curIdx + numIterations + 1) `rem` numElems
        in dropWhile ((/= newIdx) . fst) l

updateCaveHeight :: Cave -> Cave
updateCaveHeight (height, cave) = let (newCave, rs) = splitAt 50 cave in (height + length rs, newCave)

caveRockHeight :: Cave -> Int
caveRockHeight (height, cave) = height + length cave

updateCaveState :: Cache -> CaveState -> (Cache, CaveState)
updateCaveState cache state@(CaveState shapes jets _ _ _) =
  let newState = pushRock (snd $ head jets) state
   in rockFalls cache $ newState {nextJets = tail jets}

runSimulation :: Cache -> CaveState -> CaveState
runSimulation cache state = case updateCaveState cache state of
  (cache, newState@(CaveState _ _ _ _ 0)) -> newState
  (cache, newState) -> runSimulation cache newState

heightAfterSimulation :: CaveState -> Int
heightAfterSimulation = caveRockHeight . cave . runSimulation M.empty

showCave :: RockShape -> [String]
showCave = map (map caveToChar)
  where
    caveToChar Hole = '.'
    caveToChar Rock = '#'

showState :: CaveState -> [String]
showState (CaveState _ jets cave fs@((x, y), shapes) _)
  | y <= 0 = showCave (map (padRow x) shapes ++ replicate (-y) (replicate caveWidth Hole) ++ snd cave) ++ [show (head jets)]
  | otherwise = showCave (snd $ mergeRockWithCave cave fs) ++ [show (head jets)]

-- Parser

rockParser :: Parser Rock
rockParser = (Rock <$ char '#') <|> (Hole <$ char '.')

rockShapeParser :: Parser RockShape
rockShapeParser = many1 (many1 rockParser <* newline)

parseAllRocks :: IO [RockShape]
parseAllRocks = fmap (fromRight []) parseRocks
  where
    parseRocks = parseFromFile (rockShapeParser `sepBy` newline <* eof) "inputs/day17_rocks"

parseJetPattern :: Parser [Jet]
parseJetPattern = many1 (jetLeft <|> jetRight) <* newline <* eof
  where
    jetLeft = JetLeft <$ char '<'
    jetRight = JetRight <$ char '>'

part1 :: [RockShape] -> Parser Int
part1 rocks = heightAfterSimulation . initState 2022 rocks <$> parseJetPattern

part2 :: [RockShape] -> Parser Int
part2 rocks = heightAfterSimulation .  initState 1000000000000 rocks <$> parseJetPattern

showAllStates :: [CaveState] -> [String]
showAllStates = intercalate ["-------"] . map showState

main :: IO ()
main = do
  rocks <- parseAllRocks
  parseAndSolve "inputs/day17" (part1 rocks) (part2 rocks)
