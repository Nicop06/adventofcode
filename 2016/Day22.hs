module Day22
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (first, second)
import Control.Monad (void)
import Data.Array
import Text.Parsec
import Text.Parsec.String

type Coord = (Int, Int)

type Size = Int

type Usage = Int

data NodeDesc = NodeDesc
  { getCoordinates :: Coord,
    getSize :: Size,
    initialUsed :: Size,
    initialAvail :: Size,
    getUsage :: Usage
  }
  deriving (Show, Eq)

data Node = Node
  { getUsed :: Size,
    getAvail :: Size
  }
  deriving (Show, Eq)

type Grid = Array Coord Node

pairs :: Eq a => [a] -> [(a, a)]
pairs l = filter (uncurry (/=)) $ (,) <$> l <*> l

makeGrid :: [NodeDesc] -> Grid
makeGrid l =
  let (w, h) = maximum . map getCoordinates $ l
   in array ((0, 0), (w, h)) [(getCoordinates n, nodeFromDesc n) | n <- l]
  where
    nodeFromDesc :: NodeDesc -> Node
    nodeFromDesc n = Node (initialUsed n) (initialAvail n)

makeGoal :: Grid -> Coord
makeGoal g = let ((_, _), (gx, _)) = bounds g in (gx, 0)

canMoveTo :: Node -> Node -> Bool
canMoveTo from to = getUsed from > 0 && getUsed from <= getAvail to

canMoveToCoords :: Coord -> Coord -> Grid -> Bool
canMoveToCoords from to grid = canMoveTo (grid ! from) (grid ! to)

moveTo :: Coord -> Coord -> Grid -> Grid
moveTo from to grid = grid // [(from, newFromNode), (to, newToNode)]
  where
    (Node fromUsed fromAvail) = grid ! from
    (Node toUsed toAvail) = grid ! to
    newFromNode = Node 0 (fromUsed + fromAvail)
    newToNode = Node (toUsed + fromUsed) (toAvail - fromUsed)

allNeighbours :: Grid -> Coord -> [Coord]
allNeighbours g c =
  filter (inRange $ bounds g) $
    [first, second] <*> [(+ 1), subtract 1] <*> [c]

parseInput :: Parser [NodeDesc]
parseInput = count 2 skipLine *> (parseNode `sepEndBy1` newline) <* eof

skipLine :: Parser ()
skipLine = void $ many1 (noneOf "\n") <* newline

parseNode :: Parser NodeDesc
parseNode = NodeDesc <$> parseCoordinates <*> parseSize <*> parseSize <*> parseSize <*> parseUsage

parseCoordinates :: Parser Coord
parseCoordinates = (,) <$> (string "/dev/grid/node-x" *> parseNum) <*> (string "-y" *> parseNum <* many (char ' '))

parseSize :: Parser Size
parseSize = parseNum <* char 'T' <* many (char ' ')

parseUsage :: Parser Usage
parseUsage = parseNum <* char '%' <* many (char ' ')

parseNum :: Parser Int
parseNum = read <$> many1 digit

part1 :: [NodeDesc] -> IO ()
part1 = print . length . filter (uncurry canMoveTo) . pairs . elems . makeGrid

part2 :: [NodeDesc] -> IO ()
part2 n = print . filter (\(c1, c2) -> canMoveTo (grid ! c1) (grid ! c2)) . filter (uncurry isNeighbour) $ pairs (indices grid)
  where
    grid = makeGrid n
    isNeighbour (x1, y1) (x2, y2) = abs (x1 - x2) == 1 && abs (y1 - y2) == 1
