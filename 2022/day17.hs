import Data.Either (fromRight)
import Data.List (transpose)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

data Jet = JetRight | JetLeft deriving (Show)

data Rock = Hole | Rock deriving (Show, Eq)

type RockShape = [[Rock]]

type CaveHeight = [Int]

type Position = (Int, Int)

type FallingRock = (Position, RockShape)

data CaveState = CaveState {nextShapes :: [RockShape], nextJets :: [Jet], caveHeight :: CaveHeight, fallingRock :: FallingRock} deriving (Show)

caveWidth = 7 :: Int

initialCave = replicate caveWidth 0 :: CaveHeight

-- Helpers

initState :: [RockShape] -> [Jet] -> CaveState
initState shapes jets = CaveState allShapes allJets initialCave firstFallingRock
  where
    firstFallingRock = newFallingRock initialCave (head shapes)
    forever = concat . repeat
    allShapes = tail . forever $ shapes
    allJets = forever jets

newFallingRock :: CaveHeight -> RockShape -> FallingRock
newFallingRock c r = ((2, caveRockHeight c + 3), r)

caveRockHeight :: CaveHeight -> Int
caveRockHeight = maximum

pushRock :: Jet -> FallingRock -> FallingRock
pushRock JetLeft fr@((x, y), r)
  | x > 0 = ((x - 1, y), r)
  | otherwise = fr
pushRock JetRight fr@((x, y), r)
  | (x + length (head r)) < caveWidth = ((x + 1, y), r)
  | otherwise = fr

rockFalls :: CaveState -> CaveState
rockFalls (CaveState shapes jets c r@((x, y), rs)) =
  let rockHeight = fallingRockBottom r
      hasCollidingRock = or $ zipWith (==) c rockHeight
   in if hasCollidingRock
        then CaveState (tail shapes) jets (zipWith (+) (fallingRockTop r) c) (newFallingRock c (head shapes))
        else CaveState shapes jets c ((x, y - 1), rs)

updateCaveState :: CaveState -> CaveState
updateCaveState (CaveState shapes jets c r) =
  let newRock = pushRock (head jets) r
   in rockFalls (CaveState shapes (tail jets) c newRock)

fallingRockBottom :: FallingRock -> [Int]
fallingRockBottom ((x, y), r) = concat [beforeRock, rockHeight, afterRock]
  where
    beforeRock = replicate x maxBound
    afterRock = replicate (caveWidth - x - length (head r)) maxBound
    rockHeight = map (+ y) $ rockShapeBottom r

fallingRockTop :: FallingRock -> [Int]
fallingRockTop ((x, y), r) = concat [beforeRock, rockHeight, afterRock]
  where
    beforeRock = replicate x 0
    afterRock = replicate (caveWidth - x - length (head r)) 0
    rockHeight = rockShapeTop r

rockShapeBottom :: RockShape -> [Int]
rockShapeBottom = rockShapeHeight . transpose . reverse

rockShapeTop :: RockShape -> [Int]
rockShapeTop rock =
  let totalHeight = length rock
      rockHeight = rockShapeHeight $ transpose rock
   in map (totalHeight -) rockHeight

rockShapeHeight :: RockShape -> [Int]
rockShapeHeight [] = []
rockShapeHeight (r : rs) = colHeight r : rockShapeHeight rs
  where
    colHeight = fst . head . filter ((== Rock) . snd) . zip [0 ..]

runSimulation :: Int -> CaveState -> CaveState
runSimulation numSteps state = foldl (flip . const $ updateCaveState) state [1 .. numSteps]

heightAfterSimulation :: Int -> CaveState -> Int
heightAfterSimulation = (fmap . fmap) (maximum . caveHeight) runSimulation

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
part1 rocks = heightAfterSimulation 2022 . initState rocks <$> parseJetPattern

part2 = return 0

main :: IO ()
main = do
  rocks <- parseAllRocks
  parseAndSolve "/tmp/testinput" (part1 rocks) part2

-- parseAndSolve "inputs/day17" (part1 rocks) part2
