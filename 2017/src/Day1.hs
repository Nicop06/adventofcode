module Day1
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (group)
import Text.Parsec
import Text.Parsec.String

parseInput :: Parser [Int]
parseInput = many1 (read . pure <$> digit) <* newline <* eof

part1 :: [Int] -> IO ()
part1 = print . sum . concatMap (drop 1) . group . wrapList
  where
    wrapList :: [a] -> [a]
    wrapList [] = []
    wrapList (x : xs) = x : xs ++ [x]

part2 :: [Int] -> IO ()
part2 l = print . sum . map fst . filter (uncurry (==)) $ matchedElems
  where
    matchedElems = let (l1, l2) = splitAt (length l `div` 2) l in zip (l1 ++ l2) (l2 ++ l1)
