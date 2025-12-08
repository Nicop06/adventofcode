module Day4
  ( parseInput
  , part1
  , part2
  ) where

import Data.Array
import Text.Parsec
import Text.Parsec.String

data Tile
  = Free
  | Roll
  deriving (Eq, Show)

type Coord = (Int, Int)

type Grid = Array Coord Tile

type Mask = Array Coord Bool

maxNumNeighbourForAccess :: Int
maxNumNeighbourForAccess = 4

inBounds :: (Coord, Coord) -> Coord -> Bool
inBounds ((minX, minY), (maxX, maxY)) (x, y) =
  x >= minX && y >= minY && x <= maxX && y <= maxY

neighbours :: (Coord, Coord) -> Coord -> [Coord]
neighbours b (i, j) =
  filter (/= (i, j)) $
  filter (inBounds b) $ (,) <$> map (+ i) [-1, 0, 1] <*> map (+ j) [-1, 0, 1]

accessibleTiles :: Grid -> Mask
accessibleTiles grid =
  let b = bounds grid
   in array b [(i, isTileAccessible grid i) | i <- range b]

isTileAccessible :: Grid -> Coord -> Bool
isTileAccessible a i =
  let b = bounds a
   in (a ! i == Roll) &&
      (length . filter ((== Roll) . (a !)) $ neighbours b i) <
      maxNumNeighbourForAccess

numAccessibleTiles :: Grid -> Int
numAccessibleTiles = sum . map fromEnum . elems . accessibleTiles

numAccessibleTilesRec :: Grid -> Int
numAccessibleTilesRec grid =
  let mask = accessibleTiles grid
      b = bounds grid
      n = sum . map fromEnum $ elems mask
   in if n > 0
        then n +
             numAccessibleTilesRec
               (listArray b $ zipWith emptyTile (elems mask) (elems grid))
        else 0
  where
    emptyTile True _ = Free
    emptyTile _ t = t

listToGrid :: [[Tile]] -> Grid
listToGrid tiles =
  let width = length (head tiles) - 1
      height = length tiles - 1
   in listArray ((0, 0), (height, width)) $ concat tiles

parseTile :: Parser Tile
parseTile = (Free <$ char '.') <|> (Roll <$ char '@')

parseInput :: Parser Grid
parseInput = listToGrid <$> (many1 parseTile `sepEndBy1` newline) <* eof

part1 :: Grid -> IO ()
part1 = print . numAccessibleTiles

part2 :: Grid -> IO ()
part2 = print . numAccessibleTilesRec
