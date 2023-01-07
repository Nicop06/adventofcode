module Day4
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

data Range = Range {start :: Int, end :: Int} deriving (Show)

contains :: Range -> Range -> Bool
(Range s1 e1) `contains` (Range s2 e2) = s1 <= s2 && e2 <= e1

overlaps :: Range -> Range -> Bool
(Range s1 e1) `overlaps` (Range s2 e2) = Range s2 e2 `contains` Range s1 s1 || Range s1 e1 `contains` Range s2 s2

fullyContains :: Range -> Range -> Bool
fullyContains r1 r2 = r1 `contains` r2 || r2 `contains` r1

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseRange :: Parser Range
parseRange = Range <$> (parseNumber <* char '-') <*> parseNumber

parsePair :: Parser (Range, Range)
parsePair = (,) <$> (parseRange <* char ',') <*> parseRange

parseInput :: Parser [(Range, Range)]
parseInput = parsePair `sepEndBy1` newline <* eof

part1 :: [(Range, Range)] -> IO ()
part1 = print . length . filter (uncurry fullyContains)

part2 :: [(Range, Range)] -> IO ()
part2 = print . length . filter (uncurry overlaps)
