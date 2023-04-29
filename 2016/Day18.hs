module Day18
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

data TileState = S | T deriving (Eq)

type Row = [TileState]

instance Show TileState where
  show S = "."
  show T = "^"

trapPatterns :: [[TileState]]
trapPatterns = [[T, T, S], [S, T, T], [T, S, S], [S, S, T]]

nextRow :: [TileState] -> [TileState]
nextRow (a : b : c : rs) = (if [a, b, c] `elem` trapPatterns then T else S) : nextRow (b : c : rs)
nextRow _ = []

iterateRows :: Row -> [Row]
iterateRows = iterate (\r -> nextRow (S : r ++ [S]))

numSafeTiles :: Int -> Row -> Int
numSafeTiles numRows = length . filter (== S) . concat . take numRows . iterateRows

parseInput :: Parser Row
parseInput = many1 parseTile <* newline <* eof

parseTile :: Parser TileState
parseTile = (S <$ char '.') <|> (T <$ char '^')

part1 :: Row -> IO ()
part1 = print . numSafeTiles 40

part2 :: Row -> IO ()
part2 = print . numSafeTiles 400000
