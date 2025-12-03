module Day3
  ( parseInput
  , part1
  , part2
  ) where

import Text.Parsec
import Text.Parsec.String

type Battery = Int

type Bank = [Battery]

hasAtLeastLength :: [a] -> Int -> Bool
hasAtLeastLength _ 0 = True
hasAtLeastLength [] i = i == 0
hasAtLeastLength (_:xs) i = hasAtLeastLength xs (i - 1)

nextBestBattery :: Int -> Bank -> (Int, Bank)
nextBestBattery n (b:bs)
  | bs `hasAtLeastLength` (n - 1) =
    let (b', bs') = nextBestBattery n bs
     in if b >= b'
          then (b, bs)
          else (b', bs')
  | otherwise = (0, [])
nextBestBattery _ _ = (0, [])

bestBatteries :: Int -> Bank -> Bank
bestBatteries 0 _ = []
bestBatteries n bs =
  let (b, bs') = nextBestBattery n bs
   in b : bestBatteries (n - 1) bs'

bankJoltage :: Bank -> Int
bankJoltage = foldl ((+) . (* 10)) 0

totalJoltage :: Int -> [Bank] -> Int
totalJoltage numDigits = sum . map (bankJoltage . bestBatteries numDigits)

parseBank :: Parser Bank
parseBank = many1 (read . pure <$> digit)

parseInput :: Parser [Bank]
parseInput = parseBank `sepEndBy1` newline <* eof

part1 :: [Bank] -> IO ()
part1 = print . totalJoltage 2

part2 :: [Bank] -> IO ()
part2 = print . totalJoltage 12
