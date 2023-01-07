{-# LANGUAGE OverloadedStrings #-}

module Day4
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec
import Text.Parsec.String

data Range = Range {start :: Int, end :: Int} deriving (Show)

contains :: Range -> Range -> Bool
(Range s1 e1) `contains` (Range s2 e2) = s1 <= s2 && e2 <= e1

overlaps :: Range -> Range -> Bool
(Range s1 e1) `overlaps` (Range s2 e2) = Range s2 e2 `contains` Range s1 s1 || Range s1 e1 `contains` Range s2 s2

parseRange :: T.Text -> Range
parseRange range =
  case T.splitOn "-" range of
    [start, end] -> Range (read . T.unpack $ start) (read . T.unpack $ end)
    _ -> error "Range should have a start and end"

parsePair :: T.Text -> (Range, Range)
parsePair pair =
  case T.splitOn "," pair of
    [elf1, elf2] -> (parseRange elf1, parseRange elf2)
    _ -> error "We should have two elfs"

parseLines :: [String] -> [(Range, Range)]
parseLines = map (parsePair . T.pack)

countPairToConsider :: ((Range, Range) -> Bool) -> [String] -> Int
countPairToConsider pairToConsider l = length $ filter pairToConsider (parseLines l)

parseInput :: Parser [String]
parseInput = lines <$> many1 anyChar

part1 :: [String] -> IO ()
part1 = print . countPairToConsider (\(pair1, pair2) -> pair1 `contains` pair2 || pair2 `contains` pair1)

part2 :: [String] -> IO ()
part2 = print . countPairToConsider (uncurry overlaps)
