module Day1
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (sort)
import Text.Parsec
import Text.Parsec.String

type Inventory = [Int]

largestAmountCaloriesTopK :: Int -> [[Int]] -> Int
largestAmountCaloriesTopK k = sum . take k . reverse . sort . map sum

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseInventory :: Parser Inventory
parseInventory = parseNumber `sepEndBy1` newline

parseInput :: Parser [Inventory]
parseInput = parseInventory `sepEndBy1` newline <* eof

part1 :: [Inventory] -> IO ()
part1 = print . largestAmountCaloriesTopK 1

part2 :: [Inventory] -> IO ()
part2 = print . largestAmountCaloriesTopK 3
