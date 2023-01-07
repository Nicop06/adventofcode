module Day17
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Applicative (ZipList (..), getZipList)
import Data.Bifunctor (first, second)
import Data.Either (fromRight)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String

-- Data

data Jet = JetRight | JetLeft deriving (Show)

data Rock = Hole | Rock deriving (Show, Eq, Ord)

type RockShape = [[Rock]]

type Cave = (Int, RockShape)

type Position = (Int, Int)

type FallingRock = (Position, RockShape)

data CaveState = CaveState {nextShapes :: [(Int, RockShape)], nextJets :: [(Int, Jet)], cave :: Cave, fallingRock :: FallingRock, remainingSteps :: Int, numShapes :: Int, numJets :: Int} deriving (Show)

type CacheState = (Int, Int, RockShape)

type Cache = M.Map CacheState (Int, Int)

caveWidth :: Int
caveWidth = 7

initialCave :: (Int, [[Rock]])
initialCave = (-1, [replicate caveWidth Rock])

-- Helpers

initState :: Int -> [RockShape] -> [Jet] -> CaveState
initState numFalling shapes jets =
  CaveState allShapes allJets initialCave firstFallingRock numFalling (length shapes) (length jets)
  where
    firstFallingRock = newFallingRock (head shapes)
    allShapes = tail . concat . repeat $ zip [0 ..] shapes
    allJets = concat . repeat $ zip [0 ..] jets

newFallingRock :: RockShape -> FallingRock
newFallingRock shape = ((2, -3), shape)

intersectsWithCave :: Cave -> FallingRock -> Bool
intersectsWithCave _ ((_, _), []) = False
intersectsWithCave cave rock@((_, y), _) =
  let (_, shapesToIntersect) = shapesAboveAndBelowCave rock
      (caveRowsToIntersect, _) = splitAt y (snd cave)
      bothRocks r r' = (r, r') == (Rock, Rock)
   in any or $ zipWith bothRocks <$> ZipList shapesToIntersect <*> ZipList caveRowsToIntersect

mergeRockWithCave :: Cave -> FallingRock -> Cave
mergeRockWithCave (h, cave) fr@((_, y), _hape) =
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

pushRock :: Jet -> CaveState -> CaveState
pushRock JetLeft = fromMaybe <$> id <*> updateRockPosition (first (subtract 1))
pushRock JetRight = fromMaybe <$> id <*> updateRockPosition (first (+ 1))

updateRockPosition :: (Position -> Position) -> CaveState -> Maybe CaveState
updateRockPosition f state@(CaveState _ _ cave (p, rs) _ _ _) =
  let (x, y) = f p
      newRock = ((x, y), rs)
   in if x < 0 || x + (length . head $ rs) > caveWidth || intersectsWithCave cave newRock
        then Nothing
        else Just $ state {fallingRock = newRock}

rockFalls :: Cache -> CaveState -> (Cache, CaveState)
rockFalls cache state@(CaveState shapes _ cave rock steps _ _) =
  case updateRockPosition (second (+ 1)) state of
    Just newState -> (cache, newState)
    Nothing ->
      let newState =
            state
              { cave = updateCaveHeight $ mergeRockWithCave cave rock,
                fallingRock = newFallingRock (snd $ head shapes),
                nextShapes = tail shapes,
                remainingSteps = steps - 1
              }
       in getOrUpdateCache cache newState

getOrUpdateCache :: Cache -> CaveState -> (Cache, CaveState)
getOrUpdateCache cache state@(CaveState shapes jets cave _ steps _ _) =
  let cacheState = (fst . head $ shapes, fst . head $ jets, snd cave)
   in case M.lookup cacheState cache of
        Nothing -> (M.insert cacheState (caveRockHeight cave, steps) cache, state)
        Just (height, s) -> (cache, reduceState state height s)

reduceState :: CaveState -> Int -> Int -> CaveState
reduceState (CaveState shapes jets cave rock steps numShapes numJets) cachedHeight cachedSteps =
  let stepsDiff = cachedSteps - steps
      remainingSteps = steps `rem` stepsDiff
      numIterations = steps `div` stepsDiff
      heightDiff = caveRockHeight cave - cachedHeight
      totalHeight = fst cave + numIterations * heightDiff
   in CaveState shapes jets (totalHeight, snd cave) rock remainingSteps numShapes numJets

updateCaveHeight :: Cave -> Cave
updateCaveHeight (height, cave) = let (newCave, rs) = splitAt 50 cave in (height + length rs, newCave)

caveRockHeight :: Cave -> Int
caveRockHeight (height, cave) = height + length cave

updateCaveState :: Cache -> CaveState -> (Cache, CaveState)
updateCaveState cache state@(CaveState _ jets _ _ _ _ _) =
  let newState = pushRock (snd $ head jets) state
   in rockFalls cache $ newState {nextJets = tail jets}

runSimulation :: Cache -> CaveState -> [CaveState]
runSimulation cache state = case updateCaveState cache state of
  (_, newState@(CaveState _ _ _ _ 0 _ _)) -> [newState]
  (cache', newState) -> newState : runSimulation cache' newState

heightAfterSimulation :: CaveState -> Int
heightAfterSimulation = caveRockHeight . cave . last . runSimulation M.empty

-- Parser

rockParser :: Parser Rock
rockParser = (Rock <$ char '#') <|> (Hole <$ char '.')

rockShapeParser :: Parser RockShape
rockShapeParser = many1 (many1 rockParser <* newline)

parseAllRocks :: Parser [RockShape]
parseAllRocks = rockShapeParser `sepEndBy` newline

parseJetPattern :: Parser [Jet]
parseJetPattern = many1 (jetLeft <|> jetRight)
  where
    jetLeft = JetLeft <$ char '<'
    jetRight = JetRight <$ char '>'

parseInput :: Parser ([RockShape], [Jet])
parseInput = (,) <$> (parseAllRocks <* newline) <*> parseJetPattern <* newline <* eof

part1 :: ([RockShape], [Jet]) -> IO ()
part1 = print . heightAfterSimulation . uncurry (initState 2022)

part2 :: ([RockShape], [Jet]) -> IO ()
part2 = print . heightAfterSimulation . uncurry (initState 1000000000000)
