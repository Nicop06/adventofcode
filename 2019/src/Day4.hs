module Day4
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Ix
import Data.List (group)
import Text.Parsec
import Text.Parsec.String

toDigits :: Int -> [Int]
toDigits = reverse . go
  where
    go :: Int -> [Int]
    go 0 = []
    go i = let (q, r) = quotRem i 10 in r : go q

digitsAscendingOrder :: [Int] -> Bool
digitsAscendingOrder (d1 : d2 : ds) = d1 <= d2 && digitsAscendingOrder (d2 : ds)
digitsAscendingOrder _ = True

parseInput :: Parser (Int, Int)
parseInput = (,) <$> (parseInt <* char '-') <*> parseInt
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 digit

part1 :: (Int, Int) -> IO ()
part1 = print . length . filter isValidPassword . range
  where
    isValidPassword :: Int -> Bool
    isValidPassword i = twoAdjacentDigitAreTheSame && digitsAscendingOrder digits
      where
        digits = toDigits i
        twoAdjacentDigitAreTheSame = any ((> 1) . length) $ group digits

part2 :: (Int, Int) -> IO ()
part2 = print . length . filter isValidPassword . range
  where
    isValidPassword :: Int -> Bool
    isValidPassword i = twoAdjacentDigitAreTheSame && digitsAscendingOrder digits
      where
        digits = toDigits i
        twoAdjacentDigitAreTheSame = any ((== 2) . length) $ group digits
