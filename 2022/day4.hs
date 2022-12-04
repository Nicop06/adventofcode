{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import ParseAndRun

data Range = Range {start :: Int, end :: Int} deriving (Show)

contains :: Range -> Range -> Bool
(Range s1 e1) `contains` (Range s2 e2) = s1 <= s2 && e2 <= e1

overlaps :: Range -> Range -> Bool
(Range s1 e1) `overlaps` (Range s2 e2) = Range s2 e2 `contains` Range s1 s1 || Range s1 e1 `contains` Range s2 s2

parseRange :: T.Text -> Range
parseRange range =
  let [start, end] = T.splitOn "-" range
   in Range (read . T.unpack $ start) (read . T.unpack $ end)

parsePair :: T.Text -> (Range, Range)
parsePair pair =
  let [elf1, elf2] = T.splitOn "," pair
   in (parseRange elf1, parseRange elf2)

parseLines :: [String] -> [(Range, Range)]
parseLines = map (parsePair . T.pack)

countPairToConsider :: ((Range, Range) -> Bool) -> [String] -> Int
countPairToConsider pairToConsider lines = length $ filter pairToConsider (parseLines lines)

part1 :: [String] -> Int
part1 = countPairToConsider (\(pair1, pair2) -> pair1 `contains` pair2 || pair2 `contains` pair1)

part2 :: [String] -> Int
part2 = countPairToConsider (uncurry overlaps)

main :: IO ()
main = parseAndRun (T.unpack "inputs/day4") part1 part2
