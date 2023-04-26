module Day9
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

parseInput :: Parser String
parseInput = concat <$> many1 (parseMarked <|> parseText) <* newline <* eof

parseText :: Parser String
parseText = many1 $ noneOf "(\n"

parseMarker :: Parser (Int, Int)
parseMarker = (,) <$> (char '(' *> parseNum <* char 'x') <*> parseNum <* char ')'

parseNum :: Parser Int
parseNum = read <$> many1 digit

parseMarked :: Parser String
parseMarked = parseMarker >>= uncurry parseAndRepeat

parseAndRepeat :: Int -> Int -> Parser String
parseAndRepeat n r = concat . replicate r <$> count n (noneOf "\n")

part1 :: String -> IO ()
part1 = print . length

part2 :: String -> IO ()
part2 = part1
