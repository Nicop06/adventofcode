module Day24
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array
import Text.Parsec
import Text.Parsec.String

data Tile
  = Wall
  | Free
  | Location Int
  deriving (Eq, Show)

type Grid = Array (Int, Int) Tile

listToGrid :: [[Tile]] -> Grid
listToGrid tiles =
  let width = length (head tiles)
      height = length tiles
   in listArray ((0, 0), (width, height)) $ concat tiles

parseTile :: Parser Tile
parseTile =
  (Free <$ char '.')
    <|> (Wall <$ char '#')
    <|> (Location . read . pure <$> digit)

parseInput :: Parser Grid
parseInput = listToGrid <$> (many1 parseTile `sepEndBy1` newline) <* eof

part1 :: Grid -> IO ()
part1 = print

part2 :: Grid -> IO ()
part2 _ = print 1
