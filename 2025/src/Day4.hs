module Day4
  ( parseInput
  , part1
  , part2
  ) where

import Control.Monad
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector as R
import Prelude as P
import Text.Parsec
import Text.Parsec.String

data Tile
  = Free
  | Roll
  deriving (Eq, Show)

type Grid = Array V DIM2 Tile

type Mask = Array D DIM2 Bool

maxNumNeighbourForAccess :: Int
maxNumNeighbourForAccess = 4

add :: Shape sh => sh -> sh -> sh
add i1 i2 = shapeOfList $ P.zipWith (+) (listOfShape i1) (listOfShape i2)

neighbours :: DIM2 -> DIM2 -> [DIM2]
neighbours dim idx =
  filter (/= idx) $
  filter (inShape dim) $ P.map (add idx) $ ix2 <$> [-1, 0, 1] <*> [-1, 0, 1]

accessibleTiles :: Grid -> Mask
accessibleTiles grid = R.traverse grid id (isTileAccessible (extent grid))

isTileAccessible :: DIM2 -> (DIM2 -> Tile) -> DIM2 -> Bool
isTileAccessible dim arrFn idx =
  (arrFn idx == Roll) &&
  (length . filter ((== Roll) . arrFn) $ neighbours dim idx) <
  maxNumNeighbourForAccess

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

numAccessibleTiles :: Monad m => Grid -> m Int
numAccessibleTiles = sumAllP . R.map boolToInt . accessibleTiles

numAccessibleTilesRec :: Grid -> Int
numAccessibleTilesRec grid =
  let mask = accessibleTiles grid
      n = sumAllS . R.map boolToInt $ mask
   in if n > 0
        then n +
             numAccessibleTilesRec (computeS $ R.zipWith emptyTile mask grid)
        else 0
  where
    emptyTile True _ = Free
    emptyTile _ t = t

listToGrid :: [[Tile]] -> Grid
listToGrid tiles =
  let width = length (head tiles)
      height = length tiles
   in fromListVector (ix2 height width) $ concat tiles

parseTile :: Parser Tile
parseTile = (Free <$ char '.') <|> (Roll <$ char '@')

parseInput :: Parser Grid
parseInput = listToGrid <$> (many1 parseTile `sepEndBy1` newline) <* eof

part1 :: Grid -> IO ()
part1 = print <=< numAccessibleTiles

part2 :: Grid -> IO ()
part2 = print . numAccessibleTilesRec
