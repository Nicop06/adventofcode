module Day4
  ( parseInput
  , part1
  , part2
  ) where

import Prelude as P
import Control.Monad
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Vector as R
import Text.Parsec
import Text.Parsec.String


data Tile = Free | Roll deriving (Eq, Show)

type Grid = Array V DIM2 Tile

maxNumNeighbourForAccess :: Int
maxNumNeighbourForAccess = 4

add :: Shape sh => sh -> sh -> sh
add i1 i2 = shapeOfList $ P.zipWith (+) (listOfShape i1) (listOfShape i2)

neighbours :: DIM2 -> DIM2 -> [DIM2]
neighbours dim idx = filter (/= idx) $ filter (inShape dim) $ P.map (add idx) $ ix2 <$> [-1,0,1] <*> [-1,0,1]

accessibleTiles :: Grid -> Array D DIM2 Bool
accessibleTiles grid = R.traverse grid id (isTileAccessible (extent grid))

isTileAccessible :: DIM2 -> (DIM2 -> Tile) -> DIM2 -> Bool
isTileAccessible dim arrFn idx = (arrFn idx == Roll) && (length . filter ((==Roll) . arrFn) $ neighbours dim idx) < maxNumNeighbourForAccess

numAccessibleTiles :: Monad m => Grid -> m Int
numAccessibleTiles = sumAllP . R.map toInt . accessibleTiles
  where toInt True = 1
        toInt False = 0

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
part2 _ = print 2
