module Day3
  ( parseInput
  , part1
  , part2
  ) where

import Text.Parsec
import Text.Parsec.String

type Battery = Int

type Bank = [Battery]

hasAtLeastLength :: [a] -> Int -> Bool
hasAtLeastLength _ 0 = True
hasAtLeastLength [] i = i == 0
hasAtLeastLength (_:xs) i = hasAtLeastLength xs (i - 1)

maxJoltage :: Int -> Bank -> Int
maxJoltage _ [] = 0
maxJoltage 0 _ = 0
maxJoltage numDigits (x:xs)
  | xs `hasAtLeastLength` (numDigits - 1) =
    max
      (x * 10 ^ (numDigits - 1) + maxJoltage (numDigits - 1) xs)
      (maxJoltage numDigits xs)
  | otherwise = 0

totalJoltage :: Int -> [Bank] -> Int
totalJoltage numDigits = sum . map (maxJoltage numDigits)

parseBank :: Parser Bank
parseBank = many1 (read . pure <$> digit)

parseInput :: Parser [Bank]
parseInput = parseBank `sepEndBy1` newline <* eof

part1 :: [Bank] -> IO ()
part1 = print . totalJoltage 2

part2 :: [Bank] -> IO ()
part2 = print . totalJoltage 12
