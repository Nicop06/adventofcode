module Day1
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

fuelRequired :: Int -> Int
fuelRequired = subtract 2 . (`div` 3)

parseInput :: Parser [Int]
parseInput = (read <$> many1 digit) `sepEndBy1` newline <* eof

part1 :: [Int] -> IO ()
part1 = print . sum . map fuelRequired

part2 :: [Int] -> IO ()
part2 = print . sum . map (go . fuelRequired)
  where
    go :: Int -> Int
    go = sum . takeWhile (> 0) . iterate fuelRequired
