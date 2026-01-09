module Day6
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.List (groupBy, sortOn)
import Data.Map as M (elems, fromListWith)
import Text.Parsec
import Text.Parsec.String

type Coord = (Int, Int)

distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseCoord :: Parser Coord
parseCoord = (,) <$> (parseInt <* string ", ") <*> parseInt

parseInput :: Parser [Coord]
parseInput = parseCoord `sepEndBy1` newline <* eof

part1 :: [Coord] -> IO ()
part1 coords =
  print
    . maximum
    . map length
    . filter (not . any isOnEdge)
    . M.elems
    . fromListWith (++)
    . map (first head)
    . filter ((== 1) . length . fst)
    . map (closestCoord &&& pure)
    $ (,) <$> [0 .. gridW] <*> [0 .. gridH]
  where
    gridW = maximum (map fst coords)
    gridH = maximum (map snd coords)

    isOnEdge :: Coord -> Bool
    isOnEdge (x, y) = x == 0 || y == 0 || x == gridW || y == gridH

    closestCoord :: Coord -> [Int]
    closestCoord c =
      map fst
        . head
        . groupBy (\x y -> snd x == snd y)
        . sortOn snd
        $ zip [0 ..] (map (distance c) coords)

part2 :: [Coord] -> IO ()
part2 coords =
  print
    . length
    . filter ((< maxDist) . distanceToAllCoords)
    $ (,) <$> [-gridSize .. gridW + gridSize] <*> [-gridSize .. gridH + gridSize]
  where
    maxDist = 10000
    numCoords = length coords
    gridSize = maxDist `div` numCoords
    gridW = maximum (map fst coords)
    gridH = maximum (map snd coords)

    distanceToAllCoords :: Coord -> Int
    distanceToAllCoords c = sum $ map (distance c) coords
