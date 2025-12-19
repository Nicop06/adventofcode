module Day25
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

-- Data

multBy :: Int
multBy = 252533

divBy :: Int
divBy = 33554393

startingNum :: Int
startingNum = 20151125

-- Helpers

seriesIndex :: (Int, Int) -> Int
seriesIndex coords = length . takeWhile (/= coords) $ iterate nextCoord (1, 1)
  where
    nextCoord (1, col) = (col + 1, 1)
    nextCoord (row, col) = (row - 1, col + 1)

codes :: [Int]
codes = iterate ((`rem` divBy) . (* multBy)) startingNum

-- Parser

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseInput :: Parser (Int, Int)
parseInput = (,) <$> (charactersToSkip *> parseNumber <* charactersToSkip) <*> parseNumber <* char '.' <* newline <* eof
  where
    charactersToSkip = many1 (letter <|> space <|> oneOf ",.")

part1 :: (Int, Int) -> IO ()
part1 = print . (codes !!) . seriesIndex

part2 :: (Int, Int) -> IO ()
part2 = putStrLn . const "We are done!!"
