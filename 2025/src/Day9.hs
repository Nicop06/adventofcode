module Day9
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow ((***))
import Control.Monad (join)
import Data.Tuple (swap)
import Text.Parsec
import Text.Parsec.String

type TileLocation = (Int, Int)

area :: TileLocation -> TileLocation -> Int
area (x, y) (x', y') = (abs (x - x') + 1) * (abs (y - y') + 1)

allPairs :: [a] -> [(a, a)]
allPairs (x : xs) = map (x,) xs ++ allPairs xs
allPairs [] = []

isCrossing :: (TileLocation, TileLocation) -> (TileLocation, TileLocation) -> Bool
isCrossing rect@((rx, ry), (rx', ry')) line@((x, y), (x', y'))
  | rx > rx' || ry > ry' = isCrossing ((min rx rx', min ry ry'), (max rx rx', max ry ry')) line
  | x == x' =
      let minY = min y y'
          maxY = max y y'
       in x > rx && x < rx' && (minY <= ry && maxY > ry || minY < ry' && maxY >= ry')
  | y == y' = isCrossing (swapTuple rect) (swapTuple line)
  | otherwise = error "Non consecutive pair given"
  where
    swapTuple = join (***) swap

isPairInside :: [TileLocation] -> (TileLocation, TileLocation) -> Bool
isPairInside (t : ts) pair = go (t : ts ++ [t])
  where
    go :: [TileLocation] -> Bool
    go (t1 : t2 : ts') = not (isCrossing pair (t1, t2)) && go (t2 : ts')
    go _ = True
isPairInside [] _ = False

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseTileLocation :: Parser TileLocation
parseTileLocation = (,) <$> parseNumber <* char ',' <*> parseNumber

parseInput :: Parser [TileLocation]
parseInput = (parseTileLocation `sepEndBy1` newline) <* eof

part1 :: [TileLocation] -> IO ()
part1 = print . maximum . map (uncurry area) . allPairs

part2 :: [TileLocation] -> IO ()
part2 ts = print . maximum . map (uncurry area) . filter (isPairInside ts) $ allPairs ts
