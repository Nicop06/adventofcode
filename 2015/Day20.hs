module Day20
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

-- Helpers

dividers :: Int -> [Int]
dividers n = filter ((==0) . (n `rem`)) [1..n]

housePresents :: [Int]
housePresents = map (sum . map (*10) . dividers) [1..]

-- Parser

parseInput :: Parser Int
parseInput = read <$> many1 digit <* newline <* eof

part1 :: Int -> IO ()
part1 n = print . (+1) . length . takeWhile (< n) $ housePresents

part2 :: Int -> IO ()
part2 = print
--part2 n = print . (!! 786239) $ housePresents
