module Day17
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

-- Data

type Capacity = Int

maxCapacity :: Capacity
maxCapacity = 150

-- Helpers

allCombinations :: [Capacity] -> [[Capacity]]
allCombinations = filter ((== maxCapacity) . sum) . map (filter (/= 0)) . mapM possibleCapacities
  where
    possibleCapacities c = [0, c]

combinationsWithMinContainers :: [Capacity] -> [[Capacity]]
combinationsWithMinContainers capacities =
  let combinations = allCombinations capacities
      shortestCombinationLen = minimum . map length $ combinations
   in filter ((== shortestCombinationLen) . length) combinations

-- Parser

parseCapacity :: Parser Capacity
parseCapacity = read <$> many1 digit

parseInput :: Parser [Capacity]
parseInput = parseCapacity `sepEndBy1` newline <* eof

part1 :: [Capacity] -> IO ()
part1 = print . length . allCombinations

part2 :: [Capacity] -> IO ()
part2 = print . length . combinationsWithMinContainers
