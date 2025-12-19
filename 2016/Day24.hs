module Day24
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (first, second, (&&&))
import Data.Array
import Data.List (permutations)
import Data.Map as M (Map, fromList, lookup)
import Data.Maybe (fromMaybe)
import GHC.Arr
import Text.Parsec
import Text.Parsec.String

data Tile
  = Wall
  | Free
  | Location Int
  deriving (Eq, Show)

type Coord = (Int, Int)

type Grid = Array Coord Tile

type DistMatrix = Array Coord Int

isLocation :: Tile -> Bool
isLocation (Location _) = True
isLocation _ = False

getNeighbours :: (Coord, Coord) -> Coord -> [Coord]
getNeighbours b c = filter (inRange b) $ [first, second] <*> [(+ 1), subtract 1] <*> [c]

distMatrix :: Coord -> Grid -> DistMatrix
distMatrix start grid = go [start] (amap (const maxBound) grid // [(start, 0)])
  where
    go :: [Coord] -> DistMatrix -> DistMatrix
    go [] m = m
    go (c : cs) m =
      let newDist = (m ! c) + 1
          neighbours = filter ((/= Wall) . (grid !)) . filter ((> newDist) . (m !)) $ getNeighbours (bounds m) c
       in go (cs ++ neighbours) (m // map (,newDist) neighbours)

distanceToAllLocations :: Grid -> Coord -> Map Coord Int
distanceToAllLocations grid c = fromList $ filter (isLocation . (grid !) . fst) $ assocs $ distMatrix c grid

allLocations :: Grid -> [Coord]
allLocations grid = filter (isLocation . (grid !)) $ indices grid

startLocation :: Grid -> Coord
startLocation grid = head . filter ((== Location 0) . (grid !)) $ indices grid

possibleOrderings :: Grid -> [[Coord]]
possibleOrderings grid = map (startLocation grid :) (permutations nonStartLocations)
  where
    nonStartLocations :: [Coord]
    nonStartLocations = filter ((/= Location 0) . (grid !)) (allLocations grid)

minDistance :: [[Coord]] -> Grid -> Int
minDistance orderings grid = minimum $ map totalDistance orderings
  where
    allDistanceMappings :: Map Coord (Map Coord Int)
    allDistanceMappings = fromList $ map (id &&& distanceToAllLocations grid) (allLocations grid)

    totalDistance :: [Coord] -> Int
    totalDistance [] = 0
    totalDistance [_] = 0
    totalDistance (c1 : c2 : cs) = fromMaybe 0 (M.lookup c2 =<< M.lookup c1 allDistanceMappings) + totalDistance (c2 : cs)

listToGrid :: [[Tile]] -> Grid
listToGrid tiles =
  let width = length (head tiles)
      height = length tiles
   in listArray ((0, 0), (height - 1, width - 1)) $ concat tiles

parseTile :: Parser Tile
parseTile =
  (Free <$ char '.')
    <|> (Wall <$ char '#')
    <|> (Location . read . pure <$> digit)

parseInput :: Parser Grid
parseInput = listToGrid <$> (many1 parseTile `sepEndBy1` newline) <* eof

part1 :: Grid -> IO ()
part1 grid = print . minDistance (possibleOrderings grid) $ grid

part2 :: Grid -> IO ()
part2 grid = print . minDistance (map (\ord -> ord ++ [startLocation grid]) $ possibleOrderings grid) $ grid
