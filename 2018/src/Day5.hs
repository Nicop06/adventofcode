module Day5
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Char (ord, toLower)
import Text.Parsec
import Text.Parsec.String

react :: String -> String
react s =
  let s' = go s
   in if length s == length s' then s else react s'
  where
    go [] = []
    go [a] = [a]
    go (a : b : rs)
      | abs (ord a - ord b) == (ord 'a' - ord 'A') = go rs
      | otherwise = a : go (b : rs)

parseInput :: Parser String
parseInput = many1 alphaNum <* newline <* eof

part1 :: String -> IO ()
part1 = print . length . react

part2 :: String -> IO ()
part2 =
  print
    . minimum
    . map (length . react)
    . zipWith removeUnit ['a' .. 'z']
    . repeat
  where
    removeUnit :: Char -> String -> String
    removeUnit c = filter ((/= c) . toLower)
