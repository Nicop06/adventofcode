module Day2
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (tails)
import Data.Tuple (swap)
import Text.Parsec
import Text.Parsec.String

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

parseRow :: Parser [Int]
parseRow = (read <$> many1 digit) `sepBy1` tab

parseInput :: Parser [[Int]]
parseInput = parseRow `sepEndBy1` newline <* eof

part1 :: [[Int]] -> IO ()
part1 = print . sum . map checksum
  where
    checksum row = maximum row - minimum row

part2 :: [[Int]] -> IO ()
part2 = print . sum . concatMap checksum
  where
    checksum :: [Int] -> [Int]
    checksum row =
      let allPairs = (pairs row ++ map swap (pairs row))
       in map (uncurry div) $ filter ((== 0) . uncurry mod) allPairs
