module Day1
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (sort)
import Text.Parsec
import Text.Parsec.String

foldInventoryAux :: String -> [[Int]] -> [[Int]]
foldInventoryAux "" xs = [] : xs
foldInventoryAux el (x : xs) = (read el : x) : xs
foldInventoryAux _ [] = []

groupInventories :: [String] -> [[Int]]
groupInventories = foldr foldInventoryAux []

largestAmountCaloriesTopK :: Int -> [[Int]] -> Int
largestAmountCaloriesTopK k = sum . take k . reverse . sort . map sum

solution :: Int -> [String] -> Int
solution k = largestAmountCaloriesTopK k . groupInventories

parseInput :: Parser [String]
parseInput = lines <$> many1 anyChar

part1 :: [String] -> IO ()
part1 = print . solution 1

part2 :: [String] -> IO ()
part2 = print . solution 3
