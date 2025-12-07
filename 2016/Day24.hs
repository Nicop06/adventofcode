module Day24
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
  = Wall
  | Free
  | Location Int
  deriving (Eq, Show)

type Grid = Array V DIM2 Tile

type DistanceMatrix = Array V DIM2 Int

isNeighbour :: DIM2 -> DIM2 -> Bool
isNeighbour ((Z :. x) :. y) ((Z :. x') :. y') =
  (x == x') && (abs (y - y') == 1) || (y == y') && (abs (x - x') == 1)

neighbours :: DIM2 -> DIM2 -> [DIM2]
neighbours dim ((Z :. x) :. y) =
  filter
    (inShape dim)
    [ix2 x (y + 1), ix2 x (y - 1), ix2 (x + 1) y, ix2 (x - 1) y]

computeDistanceMatrix :: Grid -> DIM2 -> DistanceMatrix
computeDistanceMatrix grid start =
  let dim = extent grid
      distM = fromListVector dim (replicate (size dim) (size dim))
   in computeS $
      go
        [start]
        (delay $
         R.traverse
           distM
           id
           (\f p ->
              if p == start
                then 0
                else f p))
  where
    go [] distM = distM
    go (t:ts) distM =
      let distM' =
            R.traverse
              distM
              id
              (\f p ->
                 if isNeighbour p t && (grid ! p) /= Wall
                   then min (f p) (f t + 1)
                   else f p)
       in go
            (filter
               (\p -> distM ! p /= distM' ! p)
               (neighbours (extent distM) t) P.++
             ts)
            distM'

listToGrid :: [[Tile]] -> Grid
listToGrid tiles =
  let width = length (head tiles)
      height = length tiles
   in fromListVector (ix2 height width) $ concat tiles

parseTile :: Parser Tile
parseTile =
  (Free <$ char '.') <|> (Wall <$ char '#') <|>
  (Location . read . pure <$> digit)

parseInput :: Parser Grid
parseInput = listToGrid <$> (many1 parseTile `sepEndBy1` newline) <* eof

part1 :: Grid -> IO ()
part1 = print

part2 :: Grid -> IO ()
part2 _ = print 1
