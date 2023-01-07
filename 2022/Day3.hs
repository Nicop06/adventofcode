module Day3
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Char
import Data.List (intersect)
import Text.Parsec
import Text.Parsec.String

itemPriority :: Char -> Int
itemPriority item
  | isAsciiLower item = ord item - ord 'a' + 1
  | isAsciiUpper item = ord item - ord 'A' + 27
  | otherwise = 0

duplicateItem :: String -> Char
duplicateItem items = head $ c1 `intersect` c2
  where
    (c1, c2) = splitAt (length items `div` 2) items

solvePart2 :: [String] -> Int
solvePart2 (elf1 : elf2 : elf3 : rest) = itemPriority groupItem + solvePart2 rest
  where
    groupItem = head (elf1 `intersect` elf2 `intersect` elf3)
solvePart2 _ = 0

parseInput :: Parser [String]
parseInput = lines <$> many1 anyChar

part1 :: [String] -> IO ()
part1 = print . sum . map (itemPriority . duplicateItem)

part2 :: [String] -> IO ()
part2 = print . solvePart2
