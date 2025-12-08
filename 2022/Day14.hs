module Day14
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Monad (liftM2)
import Data.Set (Set, fromList, insert, notMember, toList, union)
import Data.Tuple (swap)
import Text.Parsec
import Text.Parsec.String

-- Data

type Position = (Int, Int)

type RockLocations = Set Position

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
    isFlowing sand = sand < lowestRock && sand /= initPos
    simulateSand (p, r) sand@(x, y)
      | sand > lowestRock = (sand, r)
      | sandBelow `notMember` r = simulateSand (p, r) sandBelow
      | sandLeft `notMember` r = simulateSand (p, r) sandLeft
      | sandRight `notMember` r = simulateSand (p, r) sandRight
      | otherwise = (sand, insert sand r)
      where
        sandBelow = (x + 1, y)
        sandLeft = (x + 1, y - 1)
        sandRight = (x + 1, y + 1)

addInfiniteLine :: RockLocations -> RockLocations
addInfiniteLine rockLocations =
  let rocks = toList rockLocations
      bottomRockHeight = (maximum . map fst $ rocks) + 2
      leftMost = (minimum . map snd $ rocks) - bottomRockHeight * 2
      rightMost = (maximum . map snd $ rocks) + bottomRockHeight * 2
      bottomRocks = rockPath (bottomRockHeight, leftMost) (bottomRockHeight, rightMost)
   in union (fromList bottomRocks) rockLocations

-- Parser

parsePosition :: Parser Position
parsePosition = (,) <$> (read <$> (many1 digit <* char ',')) <*> (read <$> many1 digit)

parsePath :: Parser [Position]
parsePath = createPath <$> parsePosition `sepBy1` string " -> " <* newline

parseInput :: Parser RockLocations
parseInput = fromList . map swap . concat <$> many1 parsePath <* eof

part1 :: RockLocations -> IO ()
part1 = print . numStoppedSand

part2 :: RockLocations -> IO ()
part2 = print . (+ 1) . numStoppedSand . addInfiniteLine
