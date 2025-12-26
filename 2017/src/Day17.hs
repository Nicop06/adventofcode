{-# LANGUAGE BangPatterns #-}

module Day17
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

spinlock :: Int -> Int -> [Int]
spinlock lastValue numCycles = go [0]
  where
    go :: [Int] -> [Int]
    go [] = []
    go l@(i : _)
      | i == lastValue = l
      | otherwise =
          let n = (numCycles + 1) `mod` length l
           in go ((i + 1) : drop n l ++ take n l)

spinlockAfterZero :: Int -> Int -> Int
spinlockAfterZero lastValue numCycles = go 1 0 0
  where
    go :: Int -> Int -> Int -> Int
    go len p !v
      | len == lastValue + 1 = v
      | otherwise =
          let n = (p + numCycles) `mod` len
           in go (len + 1) (n + 1) (if n == 0 then len else v)

parseInput :: Parser Int
parseInput = read <$> many1 digit

part1 :: Int -> IO ()
part1 = print . (!! 1) . spinlock 2017

part2 :: Int -> IO ()
part2 = print . spinlockAfterZero 50000000
