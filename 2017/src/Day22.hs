module Day22
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.Map (Map, delete, findWithDefault, fromList, insert)
import Text.Parsec
import Text.Parsec.String
import Prelude hiding (Left, Right)

data NodeState = Infected | Clean | Weakened | Flagged deriving (Show, Eq)

type Coord = (Int, Int)

type NodeMap = Map Coord NodeState

data Direction = Up | Down | Left | Right deriving (Show, Eq, Bounded, Enum)

data VirusState = VirusState
  { direction :: Direction,
    position :: Coord,
    numInfection :: Int,
    nodeMap :: NodeMap
  }
  deriving (Show)

turnLeft :: Direction -> Direction
turnLeft Up = Left
turnLeft Left = Down
turnLeft Down = Right
turnLeft Right = Up

turnRight :: Direction -> Direction
turnRight Up = Right
turnRight Left = Up
turnRight Down = Left
turnRight Right = Down

turnBack :: Direction -> Direction
turnBack Up = Down
turnBack Left = Right
turnBack Down = Up
turnBack Right = Left

move :: Direction -> Coord -> Coord
move Up = first (subtract 1)
move Down = first (+ 1)
move Left = second (subtract 1)
move Right = second (+ 1)

forward :: VirusState -> VirusState
forward state@(VirusState d p _ _) = state {position = move d p}

makeNodeMap :: [[NodeState]] -> NodeMap
makeNodeMap t =
  let w = length (head t) `div` 2
      h = length t `div` 2
   in fromList $ zip ((,) <$> [-h .. h] <*> [-w .. w]) (concat t)

parseTile :: Parser NodeState
parseTile = Infected <$ char '#' <|> Clean <$ char '.'

parseInput :: Parser NodeMap
parseInput = makeNodeMap <$> (many1 parseTile `sepEndBy1` newline <* eof)

part1 :: NodeMap -> IO ()
part1 = print . numInfection . (!! 10000) . go . VirusState Up (0, 0) 0
  where
    go :: VirusState -> [VirusState]
    go state@(VirusState d p n m) =
      state
        : case findWithDefault Clean p m of
          Infected -> go $ forward (VirusState (turnRight d) p n (delete p m))
          Clean -> go $ forward (VirusState (turnLeft d) p (n + 1) (insert p Infected m))
          s -> error $ "Invalid state " ++ show s

part2 :: NodeMap -> IO ()
part2 = print . numInfection . (!! 10000000) . go . VirusState Up (0, 0) 0
  where
    go :: VirusState -> [VirusState]
    go state@(VirusState d p n m) =
      state
        : case findWithDefault Clean p m of
          Infected -> go $ forward (VirusState (turnRight d) p n (insert p Flagged m))
          Clean -> go $ forward (VirusState (turnLeft d) p n (insert p Weakened m))
          Weakened -> go $ forward (VirusState d p (n + 1) (insert p Infected m))
          Flagged -> go $ forward (VirusState (turnBack d) p n (insert p Clean m))
