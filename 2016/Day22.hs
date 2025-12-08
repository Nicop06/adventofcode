module Day22
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Monad (void)
import Data.Array.Repa as A (Array, extent, index, traverse, (!))
import Data.Array.Repa.Index
import Data.Array.Repa.Repr.Vector (V, computeVectorS, fromListVector)
import Data.List (sortOn)
import Text.Parsec
import Text.Parsec.String

type Coordinates = (Int, Int)

type Size = Int

type Usage = Int

data NodeDesc = NodeDesc {getCoordinates :: Coordinates, getSize :: Size, initialUsed :: Size, initialAvail :: Size, getUsage :: Usage} deriving (Show, Eq)

data Node = Node {getUsed :: Size, getAvail :: Size} deriving (Show, Eq)

type Grid = Array V DIM2 Node

pairs :: Eq a => [a] -> [(a, a)]
pairs l = filter (uncurry (/=)) $ (,) <$> l <*> l

nodeFromDesc :: NodeDesc -> Node
nodeFromDesc n = Node (initialUsed n) (initialAvail n)

makeGrid :: [NodeDesc] -> Grid
makeGrid l = fromListVector (ix2 (w + 1) (h + 1)) . map nodeFromDesc . sortOn getCoordinates $ l
  where
    (w, h) = maximum . map getCoordinates $ l

allCoordinates :: Grid -> [DIM2]
allCoordinates grid = let (Z :. w :. h) = extent grid in [ix2 x y | x <- [0 .. w], y <- [0 .. h]]

canMoveTo :: Node -> Node -> Bool
canMoveTo from to = getUsed from > 0 && getUsed from <= getAvail to

canMoveToCoords :: DIM2 -> DIM2 -> Grid -> Bool
canMoveToCoords from to grid = canMoveTo (grid ! from) (grid ! to)

moveTo :: DIM2 -> DIM2 -> Grid -> Grid
moveTo from to grid = computeVectorS $ A.traverse grid id replaceNode
  where
    replaceNode f i
      | i == from = let (Node used avail) = f from in Node 0 (used + avail)
      | i == to =
          let (Node fromUsed _) = f from
              (Node toUsed toAvail) = f to
           in Node (fromUsed + toUsed) (toAvail - fromUsed)
      | otherwise = f i

allNeighbours :: Coordinates -> [Coordinates]
allNeighbours (x, y) = [(x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1)]

parseInput :: Parser [NodeDesc]
parseInput = count 2 skipLine *> (parseNode `sepEndBy1` newline) <* eof

skipLine :: Parser ()
skipLine = void $ many1 (noneOf "\n") <* newline

parseNode :: Parser NodeDesc
parseNode = NodeDesc <$> parseCoordinates <*> parseSize <*> parseSize <*> parseSize <*> parseUsage

parseCoordinates :: Parser Coordinates
parseCoordinates = (,) <$> (string "/dev/grid/node-x" *> parseNum) <*> (string "-y" *> parseNum <* many (char ' '))

parseSize :: Parser Size
parseSize = parseNum <* char 'T' <* many (char ' ')

parseUsage :: Parser Usage
parseUsage = parseNum <* char '%' <* many (char ' ')

parseNum :: Parser Int
parseNum = read <$> many1 digit

part1 :: [NodeDesc] -> IO ()
part1 = print . length . filter (uncurry canMoveTo) . pairs . map nodeFromDesc

part2 :: [NodeDesc] -> IO ()
part2 = print . makeGrid
