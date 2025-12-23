module Day9
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

data Group = Group [Group] | Garbage String deriving (Eq, Show)

groupScore :: Group -> Int
groupScore = go 1
  where
    go :: Int -> Group -> Int
    go _ (Garbage _) = 0
    go i (Group groups) = i + sum (map (go (i + 1)) groups)

garbageSize :: Group -> Int
garbageSize (Garbage g) = length g
garbageSize (Group groups) = sum $ map garbageSize groups

parseIgnored :: Parser ()
parseIgnored = skipMany (char '!' *> anyChar)

parseGarbage :: Parser Group
parseGarbage =
  Garbage
    <$> between
      (char '<' <* parseIgnored)
      (char '>')
      (many (noneOf ">" <* parseIgnored))

parseGroup :: Parser Group
parseGroup =
  Group
    <$> between
      (char '{')
      (char '}')
      ((parseGroup <|> parseGarbage) `sepBy` char ',')

parseInput :: Parser Group
parseInput = parseGroup <* newline <* eof

part1 :: Group -> IO ()
part1 = print . groupScore

part2 :: Group -> IO ()
part2 = print . garbageSize
