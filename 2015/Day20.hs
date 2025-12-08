module Day20
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

-- Helpers

primes :: [Int]
primes = 2 : [i | i <- [3 ..], and [i `mod` j /= 0 | j <- takeWhile ((<= i) . (^ 2)) primes]]

numFactors :: Int -> Int -> Int
numFactors n prime =
  length . takeWhile ((== 0) . (`rem` prime)) $ iterate (`div` prime) n

primesFactors :: Int -> [(Int, Int)]
primesFactors n =
  let primesToConsider = takeWhile ((<= n) . (^ 2)) primes
   in filter ((> 0) . fst) $ zip (map (numFactors n) primesToConsider) primesToConsider

generateDivisors :: [(Int, Int)] -> [Int]
generateDivisors [] = [1]
generateDivisors ((c, d) : rs) =
  (*) <$> generateDivisors rs <*> map (d ^) [0 .. c]

allDivisors :: Int -> [Int]
allDivisors = generateDivisors . primesFactors

-- Parser

parseInput :: Parser Int
parseInput = read <$> many1 digit <* newline <* eof

part1 :: Int -> IO ()
part1 n = print . (+ 1) . length . takeWhile (< n) $ map housePresents [1 ..]
  where
    housePresents = (* 10) . sum . allDivisors

part2 :: Int -> IO ()
part2 n = print . (+ 1) . length . takeWhile (< n) $ map housePresents [1 ..]
  where
    housePresents h = (* 11) . sum . filter ((>= h) . (* 50)) $ allDivisors h
