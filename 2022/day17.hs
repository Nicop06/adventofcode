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

type Cave = [[Rock]]

type Position = (Int, Int)

type FallingRock = (Position, RockShape)

data CaveState = CaveState {nextShapes :: [RockShape], nextJets :: [Jet], cave :: Cave, fallingRock :: FallingRock} deriving (Show)

caveWidth = 7 :: Int

initialCave = [replicate caveWidth Rock]

-- Helpers

initState :: Int -> [RockShape] -> [Jet] -> CaveState
initState numFalling shapes jets =
  CaveState allShapes allJets initialCave firstFallingRock
  where
    firstFallingRock = newFallingRock (head shapes)
    forever = concat . repeat
    allShapes = take numFalling . tail . concat . repeat $ shapes
    allJets = concat . repeat $ jets

newFallingRock :: RockShape -> FallingRock
newFallingRock shape = ((2, -3), shape)

intersectsWithCave :: Cave -> FallingRock -> Bool
intersectsWithCave _ ((_, _), []) = False
intersectsWithCave cave rock@((_, y), _) =
  let (_, shapesToIntersect) = shapesAboveAndBelowCave rock
      (caveRowsToIntersect, _) = splitAt y cave
      bothRocks x y = (x, y) == (Rock, Rock)
   in any or $ zipWith bothRocks <$> ZipList shapesToIntersect <*> ZipList caveRowsToIntersect

mergeRockWithCave :: Cave -> FallingRock -> Cave
mergeRockWithCave cave fr@((x, y), shape) =
  let (rocksToAdd, shapeToMerge) = shapesAboveAndBelowCave fr
      (caveRowsToMerge, rs) = splitAt y cave
      mergedRows = getZipList $ zipWith max <$> ZipList caveRowsToMerge <*> ZipList shapeToMerge
   in rocksToAdd ++ mergedRows ++ rs

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
  let (_, rs) = splitAt i cave
   in case rs of
        row : _ -> row
        [] -> replicate caveWidth Hole

pushRock :: Jet -> CaveState -> CaveState
pushRock JetLeft = fromMaybe <$> id <*> updateRockPosition (first (subtract 1))
pushRock JetRight = fromMaybe <$> id <*> updateRockPosition (first (+ 1))

updateRockPosition :: (Position -> Position) -> CaveState -> Maybe CaveState
updateRockPosition f state@(CaveState shapes jets cave r@(p, rs)) =
  let (x, y) = f p
      newRock = ((x, y), rs)
   in if x < 0 || x + (length . head $ rs) > caveWidth || intersectsWithCave cave newRock
        then Nothing
        else Just $ state {fallingRock = newRock}

rockFalls :: CaveState -> CaveState
rockFalls state@(CaveState shapes jets cave rock) =
  case updateRockPosition (second (+ 1)) state of
    Just newState -> newState
    Nothing ->
      let newCave = mergeRockWithCave cave rock
       in state
            { cave = newCave,
              fallingRock = newFallingRock (head shapes),
              nextShapes = tail shapes
            }

caveRockHeight :: Cave -> Int
caveRockHeight = subtract 1 . length

updateCaveState :: CaveState -> CaveState
updateCaveState state@(CaveState shapes jets c r) =
  let newState = pushRock (head jets) state
   in rockFalls $ newState {nextJets = tail jets}

runSimulation :: CaveState -> [CaveState]
runSimulation state = case updateCaveState state of
  newState@(CaveState [] _ _ _) -> [newState]
  newState -> newState : runSimulation newState

heightAfterSimulation :: CaveState -> Int
heightAfterSimulation = caveRockHeight . cave . last . runSimulation

showCave :: Cave -> [String]
showCave = map (map caveToChar)
  where
    caveToChar Hole = '.'
    caveToChar Rock = '#'

showState :: CaveState -> [String]
showState (CaveState _ jets cave fs@((x, y), shapes))
  | y <= 0 = showCave (map (padRow x) shapes ++ replicate (-y) (replicate caveWidth Hole) ++ cave) ++ [show (head jets)]
  | otherwise = showCave (mergeRockWithCave cave fs) ++ [show (head jets)]

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
