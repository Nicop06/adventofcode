module Day6
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (group, sort, sortOn, transpose)
import Text.Parsec
import Text.Parsec.String

sortLettersByFrequency :: [Char] -> [Char]
sortLettersByFrequency = map head . sortOn length . group . sort

parseInput :: Parser [String]
parseInput = many1 alphaNum `sepEndBy1` newline <* eof

part1 :: [String] -> IO ()
part1 = putStrLn . map (last . sortLettersByFrequency) . transpose

part2 :: [String] -> IO ()
part2 = putStrLn . map (head . sortLettersByFrequency) . transpose
