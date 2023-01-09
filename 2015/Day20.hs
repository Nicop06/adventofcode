module Day20
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

-- Helpers

numFactors :: Int -> Int -> Int
numFactors n prime =
  length . takeWhile ((== 0) . (`rem` prime)) $ iterate (`div` prime) n

dividorsAndPrime :: Int -> [Int] -> ([Int], [Int])
dividorsAndPrime n primes =
  let
    primesToConsider = takeWhile ((<= n) . (^2)) primes
    primeFactors = filter ((> 0) . fst) $ zip (map (numFactors n) primesToConsider) primesToConsider
   in case primeFactors of
        [] -> ([1, n], n : primes)
        l -> (generateDivisors l, primes)

generateDivisors :: [(Int, Int)] -> [Int]
generateDivisors [] = [1]
generateDivisors ((c, d) : rs) =
  (*) <$> generateDivisors rs <*> map (d ^) [0 .. c]

nextHousePresents :: ([Int], [Int]) -> ([Int], [Int])
nextHousePresents (factors, primes) = dividorsAndPrime (last factors + 1) primes

-- Parser

parseInput :: Parser Int
parseInput = read <$> many1 digit <* newline <* eof

part1 :: Int -> IO ()
part1 n = print . (+ 1) . length . takeWhile (< n) $ housePresents
  where
    housePresents = map (sum . map (* 10) . fst) $ iterate nextHousePresents ([1], [])

part2 :: Int -> IO ()
part2 n = print . (+ 1) . length . takeWhile (< n) $ housePresents
  where
    housePresents = map (sum . map (* 10) . fst) $ iterate nextHousePresents ([1], [])

-- part2 n = print . (!! 786240) $ housePresents
