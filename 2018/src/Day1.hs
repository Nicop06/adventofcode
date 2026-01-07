module Day1
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Set
import Text.Parsec
import Text.Parsec.String

parseInput :: Parser [Int]
parseInput = (read <$> (optional (char '+') *> many1 (digit <|> char '-'))) `sepEndBy1` newline <* eof

part1 :: [Int] -> IO ()
part1 = print . sum

part2 :: [Int] -> IO ()
part2 = print . go empty 0 . cycle
  where
    go :: Set Int -> Int -> [Int] -> Int
    go _ _ [] = 0
    go set n (i : is)
      | n `member` set = n
      | otherwise = go (insert n set) (n + i) is
