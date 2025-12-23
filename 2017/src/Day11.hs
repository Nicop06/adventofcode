module Day11
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (second, (***))
import Text.Parsec
import Text.Parsec.String

data Direction = SW | NW | SE | NE | S | N deriving (Show, Eq)

parseDirection :: Parser Direction
parseDirection =
  try (SW <$ string "sw")
    <|> try (SE <$ string "se")
    <|> try (NE <$ string "ne")
    <|> try (NW <$ string "nw")
    <|> try (N <$ string "n")
    <|> try (S <$ string "s")

type Coord = (Int, Int)

move :: Direction -> Coord -> Coord
move N = second (+ 2)
move S = second (subtract 2)
move NE = (+ 1) *** (+ 1)
move SE = (+ 1) *** subtract 1
move NW = subtract 1 *** (+ 1)
move SW = subtract 1 *** subtract 1

distanceToCenter :: Coord -> Int
distanceToCenter (x, y) = (abs x + abs y) `div` 2

parseInput :: Parser [Direction]
parseInput = parseDirection `sepEndBy1` char ',' <* newline <* eof

part1 :: [Direction] -> IO ()
part1 = print . distanceToCenter . foldr move (0, 0)

part2 :: [Direction] -> IO ()
part2 = print . maximum . map distanceToCenter . scanr move (0, 0) . reverse
