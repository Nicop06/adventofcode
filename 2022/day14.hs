import Control.Monad (liftM2)
import Data.List (nub)
import Data.Tuple (swap)
import qualified Data.Set as Set
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type Position = (Int, Int)

type RockLocations = Set.Set Position

-- Helpers

rockPath :: Position -> Position -> [Position]
rockPath (x1, y1) (x2, y2) = liftM2 (,) (range x1 x2) (range y1 y2)
  where
    range a b
      | a <= b = [a .. b]
      | otherwise = range b a

createPath :: [Position] -> [Position]
createPath (p1 : p2 : rs) = rockPath p1 p2 ++ createPath (p2 : rs)
createPath p = p

numStoppedSand :: RockLocations -> Int
numStoppedSand rocks = length . takeWhile (isFlowing . fst) . tail . scanl simulateSand (initPos, rocks) $ repeat initPos
  where
    lowestRock = maximum rocks
    initPos = (0, 500)
    isFlowing sand = sand < lowestRock  && sand /= initPos
    simulateSand (p, rocks) sand@(x, y)
      | sand > lowestRock = (sand, rocks)
      | sandBelow `Set.notMember` rocks = simulateSand (p, rocks) sandBelow
      | sandLeft `Set.notMember` rocks = simulateSand (p, rocks) sandLeft
      | sandRight `Set.notMember` rocks = simulateSand (p, rocks) sandRight
      | otherwise = (sand, Set.insert sand rocks)
      where
        sandBelow = (x + 1, y)
        sandLeft = (x + 1, y - 1)
        sandRight = (x + 1, y + 1)

addInfiniteLine :: RockLocations -> RockLocations
addInfiniteLine rockLocations = 
    let rocks = Set.toList rockLocations
        bottomRockHeight = (maximum . map fst $ rocks) + 2
        leftMost = (minimum . map snd $ rocks) - bottomRockHeight * 2
        rightMost = (maximum . map snd $ rocks) + bottomRockHeight * 2
        bottomRocks = rockPath (bottomRockHeight, leftMost) (bottomRockHeight, rightMost)
        in Set.union (Set.fromList bottomRocks) rockLocations

-- Parser

parsePosition :: Parser Position
parsePosition = (,) <$> (read <$> (many1 digit <* char ',')) <*> (read <$> many1 digit)

parsePath :: Parser [Position]
parsePath = createPath <$> parsePosition `sepBy1` string " -> " <* newline

parseInput :: Parser RockLocations
parseInput = Set.fromList . map swap . concat <$> many1 parsePath <* eof

part1 :: Parser Int
part1 = numStoppedSand <$> parseInput

part2 :: Parser Int
part2 = (+1) . numStoppedSand . addInfiniteLine <$> parseInput

main :: IO ()
main = parseAndSolve "inputs/day14" part1 part2
