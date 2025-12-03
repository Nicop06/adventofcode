module Day3
  ( parseInput
  , part1
  , part2
  ) where

import Text.Parsec
import Text.Parsec.String

type Battery = Int

type Bank = [Battery]

allJoltage :: Int -> Bank -> [[Int]]
allJoltage _ [] = [[]]
allJoltage 0 _ = [[]]
allJoltage numDigits (b:bs) = allJoltage numDigits bs ++ map (b:) (allJoltage (numDigits - 1) bs)

sumJoltage :: [Int] -> Int
sumJoltage = foldl ((+) . (*10)) 0

maxJoltage :: Int -> Bank -> [Int]
maxJoltage _ [] = []
maxJoltage 0 _ = []
maxJoltage numDigits (b:bs) = foldr max [] (allJoltage numDigits bs ++ map (b:) (allJoltage (numDigits - 1) bs))

totalJoltage :: Int -> [Bank] -> Int
totalJoltage numDigits = sum . map (foldr (max . sumJoltage) 0 . allJoltage numDigits)

parseBank :: Parser Bank
parseBank = many1 (read .pure <$> digit)

parseInput :: Parser [Bank]
parseInput = parseBank `sepEndBy1` newline <* eof

part1 :: [Bank] -> IO ()
part1 = print . totalJoltage 2

part2 :: [Bank] -> IO ()
part2 = print . totalJoltage 12
