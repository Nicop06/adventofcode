module Day15
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import qualified Data.Map as M
import Data.Maybe (fromJust)
import IntCode
import Text.Parsec.String

data Direction
  = North
  | South
  | West
  | East
  deriving (Enum, Bounded, Show)

data Output
  = HitWall
  | Moved
  | ReachedGoal
  deriving (Enum, Eq, Show)

type Program = [Int] -> [Int]

data Tile
  = Wall
  | Empty
      { _distance :: Int,
        _robotState :: Program
      }

type Coord = (Int, Int)

type AreaMap = M.Map Coord Tile

updateState :: Int -> Program -> Program
updateState command program l = tail $ program (command : l)

neighbours :: Coord -> [(Direction, Coord)]
neighbours c = map (id &&& (`move` c)) [minBound ..]

move :: Direction -> Coord -> Coord
move North = second (+ 1)
move South = second (subtract 1)
move East = first (+ 1)
move West = first (subtract 1)

isWall :: Tile -> Bool
isWall Wall = True
isWall _ = False

explore :: AreaMap -> (Maybe Coord, AreaMap)
explore = go Nothing
  where
    nextUnexplored :: AreaMap -> [([(Direction, Coord)], Tile)]
    nextUnexplored area =
      filter (not . null . fst)
        . map (first unexploredNeighbours)
        . M.assocs
        . M.filter (not . isWall)
        $ area
      where
        unexploredNeighbours :: Coord -> [(Direction, Coord)]
        unexploredNeighbours = filter (not . (`M.member` area) . snd) . neighbours

    go :: Maybe Coord -> AreaMap -> (Maybe Coord, AreaMap)
    go goal area =
      case nextUnexplored area of
        (((direction, nextPos) : _, tile) : _) ->
          case tile of
            Wall -> error "Hit a wall. Ouch!"
            Empty distance state ->
              let command = fromEnum direction + 1 :: Int
                  output = toEnum $ head (state [command]) :: Output
                  emptyTile =
                    Empty
                      { _distance = distance + 1,
                        _robotState = updateState command state
                      }
                  (newGoal, newTile) = case output of
                    HitWall -> (goal, Wall)
                    ReachedGoal -> (Just nextPos, emptyTile)
                    Moved -> (goal, emptyTile)
               in go newGoal (M.insert nextPos newTile area)
        _ -> (goal, area)

initArea :: Program -> AreaMap
initArea program =
  M.singleton
    (0, 0)
    ( Empty {_distance = 0, _robotState = program}
    )

parseInput :: Parser [Int]
parseInput = parseProgram

part1 :: [Int] -> IO ()
part1 = print . distanceToGoal . explore . initArea . runProgram
  where
    distanceToGoal :: (Maybe Coord, AreaMap) -> Int
    distanceToGoal (goal, area) =
      case goal >>= (`M.lookup` area) of
        Just (Empty d _) -> d
        _ -> error "No goal found"

part2 :: [Int] -> IO ()
part2 = print . maxDistanceToGoal . explore . initArea . runProgram
  where
    distance :: Tile -> Int
    distance Wall = 0
    distance (Empty d _) = d

    maxDistanceToGoal :: (Maybe Coord, AreaMap) -> Int
    maxDistanceToGoal (goal, area) =
      case goal >>= (`M.lookup` area) of
        Just (Empty _ state) ->
          maximum
            . map distance
            . M.elems
            . snd
            . explore
            $ M.singleton
              (fromJust goal)
              ( Empty {_distance = 0, _robotState = state}
              )
        _ -> error "No goal found"
