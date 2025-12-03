module Day3
  ( parseInput
  , part1
  , part2
  ) where

import Text.Parsec
import Text.Parsec.String

type Battery = Int

type Bank = [Battery]

maxFirstJoltage :: Bank -> (Int, Bank)
maxFirstJoltage (j1:j2:rs) = let (max', rs') = maxFirstJoltage (j2:rs) in
  if max' > j1 then (max', rs') else (j1, j2:rs)
maxFirstJoltage _ = (0, [])

maxJoltage :: Bank -> Int
maxJoltage bank = let (m, rs) = maxFirstJoltage bank in m * 10 + foldr max 0 rs

allJoltage :: Bank -> [Int]
allJoltage (b:rs) = map (+10 * b) rs ++ allJoltage rs
allJoltage _ = []

parseBank :: Parser Bank
parseBank = many1 (read .pure <$> digit)

parseInput :: Parser [Bank]
parseInput = parseBank `sepEndBy1` newline <* eof

part1 :: [Bank] -> IO ()
part1 = print . sum . map maxJoltage

part2 :: [Bank] -> IO ()
part2 = print . sum . map (foldr max 0 . allJoltage)
