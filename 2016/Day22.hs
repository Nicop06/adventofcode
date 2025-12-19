module Day22
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (first, second)
import Control.Monad (void)
import Data.Array
import Data.Maybe (isNothing)
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

emptyNode :: Grid -> Coord
emptyNode = fst . head . filter ((== 0) . getUsed . snd) . assocs

canMoveTo :: Node -> Node -> Bool
canMoveTo from to = getUsed from > 0 && getUsed from <= getAvail to

moveFromTo :: Coord -> Coord -> Grid -> Grid
moveFromTo from to grid
  | not (canMoveTo (grid ! from) (grid ! to)) =
      error $
        "Cannot move from "
          ++ show from
          ++ " ("
          ++ show (grid ! from)
          ++ ") to "
          ++ show to
          ++ " ("
          ++ show (grid ! to)
          ++ ")"
  | otherwise = grid // [(from, newFromNode), (to, newToNode)]
  where
    (Node fromUsed fromAvail) = grid ! from
    (Node toUsed toAvail) = grid ! to
    newFromNode = Node 0 (fromUsed + fromAvail)
    newToNode = Node (toUsed + fromUsed) (toAvail - fromUsed)

allNeighbours :: Array Coord a -> Coord -> [Coord]
allNeighbours g c =
  filter (inRange $ bounds g) $
    [first, second] <*> [(+ 1), subtract 1] <*> [c]

shortestPath :: Grid -> Coord -> Coord -> Coord -> [Coord]
shortestPath grid start end goal =
  let parents = go [start] (array (bounds grid) $ map (,Nothing) (indices grid))
   in path end parents
  where
    go :: [Coord] -> Array Coord (Maybe Coord) -> Array Coord (Maybe Coord)
    go [] parents = parents
    go (c : cs) parents
      | c == end = parents
      | otherwise = go (cs ++ possibleNeighbours) $ parents // [(n, Just c) | n <- possibleNeighbours]
      where
        possibleNeighbours = filter (isNothing . (parents !)) . filter (/= goal) . filter (canFit grid c) $ allNeighbours grid c

    path :: Coord -> Array Coord (Maybe Coord) -> [Coord]
    path c parents
      | c == start = [c]
      | otherwise = case parents ! c of
          Nothing -> [c]
          Just p -> path p parents ++ [c]

canFit :: Grid -> Coord -> Coord -> Bool
canFit grid dest src =
  let (Node destUsed destAvail) = grid ! dest
      (Node srcUsed _) = grid ! src
   in srcUsed < destAvail + destUsed

moveGoalLeft :: Coord -> Grid -> (Int, Coord, Grid)
moveGoalLeft goal grid = (length pathToEmpty, newGoal, newGrid)
  where
    newGoal = first (subtract 1) goal
    pathToEmpty = shortestPath grid (emptyNode grid) newGoal goal

    updateGrid :: [Coord] -> Grid -> Grid
    updateGrid (c1 : c2 : cs) g = updateGrid (c2 : cs) (moveFromTo c2 c1 g)
    updateGrid _ g = g

    newGrid = moveFromTo goal newGoal $ updateGrid pathToEmpty grid

numStepsToMoveGoalToStart :: Coord -> Grid -> Int
numStepsToMoveGoalToStart goal grid =
  let (steps, newGoal, newGrid) = moveGoalLeft goal grid
   in if newGoal == (0, 0)
        then steps
        else steps + numStepsToMoveGoalToStart newGoal newGrid

parseInput :: Parser Grid
parseInput = makeGrid <$> (count 2 skipLine *> (parseNode `sepEndBy1` newline) <* eof)

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

part1 :: Grid -> IO ()
part1 = print . length . filter (uncurry canMoveTo) . pairs . elems

part2 :: Grid -> IO ()
part2 grid = print $ numStepsToMoveGoalToStart goal grid
  where
    goal = let ((_, _), (gx, _)) = bounds grid in (gx, 0)
