module Day3
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (first, second, (***))
import Data.Map as M (Map, empty, insert, lookup)
import Data.Maybe (mapMaybe)
import Text.Parsec
import Text.Parsec.String

type Coord = (Int, Int)

nextCoord :: Coord -> Coord
nextCoord (x, y)
  | x == y = if x >= 0 then (x + 1, y) else (x, y + 1)
  | x == (-y) = (x - signum x, y)
  | abs y < abs x = (x, y - signum x)
  | abs y > abs x = (x + signum y, y)
  | otherwise = error $ "Impossible next coordination for " ++ show (x, y)

neighbours :: Coord -> [Coord]
neighbours c = (***) <$> transforms <*> transforms <*> [c]
  where
    transforms = [(+ 1), id, subtract 1]

-- Operations to perform on the coordinates to move around the square clockwise.
moveClockwiseSquare :: [Int -> (Int, Int) -> (Int, Int)]
moveClockwiseSquare = [first . subtract, second . subtract, first . (+), second . (+)]

manhattanDistance :: Coord -> Int
manhattanDistance (x, y) = abs x + abs y

squareCornerCoordinate :: Int -> Coord
squareCornerCoordinate s = let x = (s - 1) `div` 2 in (x, x)

closestOddSqrt :: Int -> Int
closestOddSqrt i =
  if even closestSqrt then closestSqrt + 1 else closestSqrt
  where
    closestSqrt :: Int
    closestSqrt = ceiling $ sqrt (fromIntegral i)

dataCoordinate :: Int -> Coord
dataCoordinate d = foldr ($) (squareCornerCoordinate sq) coordOperations
  where
    sq = closestOddSqrt d
    distToCorner = sq * sq - d
    sideLength = sq - 1
    (q, r) = divMod sideLength distToCorner
    coordOperations = (moveClockwiseSquare !! q) r : (take q moveClockwiseSquare <*> [sideLength])

parseInput :: Parser Int
parseInput = read <$> many1 digit <* newline <* eof

part1 :: Int -> IO ()
part1 = print . manhattanDistance . dataCoordinate

part2 :: Int -> IO ()
part2 i = print $ go M.empty (0, 0)
  where
    go :: Map Coord Int -> Coord -> Int
    go m c =
      let n = max 1 (sum $ mapMaybe (`M.lookup` m) (neighbours c))
       in if n > i then n else go (insert c n m) (nextCoord c)
