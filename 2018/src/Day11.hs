module Day11
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Monad
import Control.Monad.ST (ST)
import Data.Array.Base
import Data.Array.ST
import Data.List (maximumBy)
import Data.Ord (comparing)
import Text.Parsec
import Text.Parsec.String

type Coord = (Int, Int)

type SquareCoord = (Int, Int, Int)

type GridPower = UArray Coord Int

squarePower :: GridPower -> SquareCoord -> Int
squarePower gridPower (x, y, s) =
  gridPower ! (x + s, y + s)
    - gridPower ! (x, y + s)
    - gridPower ! (x + s, y)
    + gridPower ! (x, y)

-- Compute the power of all the sub-rectangle of the grid starting at the origin.
-- grid ! (x, y) represents the total power of all the cells between (1, 1) and (x, y)
computeGridPower :: Int -> GridPower
computeGridPower serialNum = runSTUArray go
  where
    cellPower :: Coord -> Int
    cellPower (x, y) =
      let rackId = x + 10
          powerLevel = (y * rackId + serialNum) * rackId
       in ((powerLevel `div` 100) `mod` 10) - 5

    gridBounds :: (Coord, Coord)
    gridBounds = ((1, 1), (300, 300))

    go :: ST s (STUArray s Coord Int)
    go = do
      gridPower <- newArray ((0, 0), (300, 300)) 0
      forM_ (range gridBounds) $ \c@(x, y) -> do
        top <- readArray gridPower (x, y - 1)
        left <- readArray gridPower (x - 1, y)
        topLeft <- readArray gridPower (x - 1, y - 1)
        writeArray gridPower c (cellPower c + top + left - topLeft)
      return gridPower

bestSquare :: (SquareCoord, SquareCoord) -> GridPower -> SquareCoord
bestSquare r gridPower =
  let (x, y, s) = argMax (squarePower gridPower) $ filter inGridRange $ range r
   in (x + 1, y + 1, s)
  where
    inGridRange :: SquareCoord -> Bool
    inGridRange (x, y, size) = x + size - 1 < 300 && y + size - 1 < 300

argMax :: Ord b => (a -> b) -> [a] -> a
argMax f = maximumBy (comparing f)

parseInput :: Parser GridPower
parseInput = computeGridPower . read <$> many1 digit

part1 :: GridPower -> IO ()
part1 = print . bestSquare ((1, 1, 3), (300, 300, 3))

part2 :: GridPower -> IO ()
part2 = print . bestSquare ((1, 1, 1), (300, 300, 300))
