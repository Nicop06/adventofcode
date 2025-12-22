module Day6
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Map as M (Map, empty, insert, lookup)
import Text.Parsec
import Text.Parsec.String

distribute :: Int -> Int -> [Int]
distribute n k =
  let (q, r) = k `divMod` n
   in [q + (if i < r then 1 else 0) | i <- [0 .. (n - 1)]]

extractMax :: [Int] -> (Int, Int, [Int])
extractMax = go 0
  where
    go :: Int -> [Int] -> (Int, Int, [Int])
    go i [] = (0, i, [])
    go i (x : xs) =
      let (y, j, ys) = go (i + 1) xs
       in if y > x then (y, j, x : ys) else (x, i, 0 : xs)

numSteps :: [Int] -> (Int, Int)
numSteps = go 0 M.empty
  where
    go :: Int -> Map [Int] Int -> [Int] -> (Int, Int)
    go step cache banks = case M.lookup banks cache of
      Just n -> (step, step - n)
      Nothing -> go (step + 1) (insert banks step cache) newBanks
        where
          (k, i, banksWithoutMax) = extractMax banks
          (start, end) = splitAt (length banks - i - 1) $ distribute (length banks) k
          newBanks = zipWith (+) (end ++ start) banksWithoutMax

parseInput :: Parser [Int]
parseInput = (read <$> many1 (digit <|> char '-')) `sepBy1` tab <* newline <* eof

part1 :: [Int] -> IO ()
part1 = print . fst . numSteps

part2 :: [Int] -> IO ()
part2 = print . snd . numSteps
