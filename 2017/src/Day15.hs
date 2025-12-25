{-# LANGUAGE NumericUnderscores #-}

module Day15 (
  parseInput,
  part1,
  part2,
)
where

import Control.Arrow ((***))
import Data.Bits
import Text.Parsec
import Text.Parsec.String

divider :: Int
divider = 2_147_483_647

generator :: Int -> (Int -> Bool) -> Int -> Int
generator f cond = head . filter cond . drop 1 . iterate ((`mod` divider) . (* f))

generatorA :: (Int -> Bool) -> Int -> Int
generatorA = generator 16_807

generatorB :: (Int -> Bool) -> Int -> Int
generatorB = generator 48_271

last16Bits :: Int -> Int
last16Bits = (.&.) (2 ^ 16 - 1)

numMatching :: Int -> [(Int, Int)] -> Int
numMatching n =
  length
    . filter (uncurry (==))
    . map (last16Bits *** last16Bits)
    . take n

parseInput :: Parser (Int, Int)
parseInput = (,) <$> parseGeneratorValue "A" <*> parseGeneratorValue "B" <* eof
 where
  parseGeneratorValue :: String -> Parser Int
  parseGeneratorValue g =
    read
      <$> (string ("Generator " ++ g ++ " starts with ") *> many1 digit <* newline)

part1 :: (Int, Int) -> IO ()
part1 =
  print
    . numMatching 40_000_000
    . iterate (generatorA (const True) *** generatorB (const True))

part2 :: (Int, Int) -> IO ()
part2 =
  print
    . numMatching 5_000_000
    . iterate (generatorA ((== 0) . (`mod` 4)) *** generatorB ((== 0) . (`mod` 8)))
