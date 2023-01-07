module Day1
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

parseChar :: Parser Int
parseChar = (1 <$ char '(') <|> (-1 <$ char ')')

parseInput :: Parser [Int]
parseInput = many1 parseChar <* eof

part1 :: [Int] -> IO ()
part1 = print . sum

part2 :: [Int] -> IO ()
part2 = print . length . takeWhile (>= 0) . scanl (+) 0
