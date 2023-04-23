module Day3
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String
import Data.List (transpose)

data Triangle = Triangle Int Int Int deriving (Show, Eq)

isValid :: Triangle -> Bool
isValid (Triangle x y z) = x + y > z && x + z > y && y + z > x

groupTriangles :: [Int] -> [Triangle]
groupTriangles (x : y : z : rs) = Triangle x y z : groupTriangles rs
groupTriangles _ = []

parseInput :: Parser [[Int]]
parseInput = many1 parseLines <* eof
    where parseLines = count 3 parseCoord <* newline
          parseCoord = read <$> (spaces *> many1 digit)

part1 :: [[Int]] -> IO ()
part1 = print . length . filter isValid . groupTriangles . concat

part2 :: [[Int]] -> IO ()
part2 = part1 . transpose
