module Day3
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (transpose)
import Text.Parsec
import Text.Parsec.String

data Triangle = Triangle Int Int Int deriving (Show, Eq)

isValid :: Triangle -> Bool
isValid (Triangle x y z) = x + y > z && x + z > y && y + z > x

groupTriangles :: [Int] -> [Triangle]
groupTriangles (x : y : z : rs) = Triangle x y z : groupTriangles rs
groupTriangles _ = []

parseInput :: Parser [[Int]]
parseInput = parseLine `sepEndBy1` newline <* eof
  where
    parseLine = count 3 parseCoord
    parseCoord = read <$> (spaces *> many1 digit)

part1 :: [[Int]] -> IO ()
part1 = print . length . filter isValid . groupTriangles . concat

part2 :: [[Int]] -> IO ()
part2 = part1 . transpose
