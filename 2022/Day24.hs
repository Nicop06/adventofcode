module Day24
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (first, second)
import Data.List (nub)
import Data.Set as S (fromList, notMember)
import Text.Parsec
import Text.Parsec.String

-- Data

data Tile = Open | Wall | Bliz Direction deriving (Show, Eq)

type Position = (Int, Int)

type Size = (Int, Int)

data Direction = U | D | L | R deriving (Show, Eq)

data Blizzard = Blizzard {blizzardDir :: Direction, getBlizzardPos :: Position} deriving (Show)

data GridState = GridState {getBlizzards :: [Blizzard], getPossiblePos :: [Position]} deriving (Show)

data Grid = Grid {getGridSize :: Size, getStart :: Position, getEnd :: Position}

data Goal = Start | End

-- Helpers

initialState :: [[Tile]] -> (Grid, GridState)
initialState tiles =
  let gridSize@(r, c) = (length tiles - 2, length (head tiles) - 2)
      start = (-1, 0)
      end = (r, c - 1)
      tiles' = tail . init . map (tail . init) $ tiles
   in (Grid gridSize start end, GridState (initialBlizzards tiles' (0, 0)) [start])

initialBlizzards :: [[Tile]] -> Position -> [Blizzard]
initialBlizzards [] _ = []
initialBlizzards ([] : cs) (x, _) = initialBlizzards cs (x + 1, 0)
initialBlizzards ((t : rs) : cs) (x, y) = maybeAddBlizzard t (x, y) $ initialBlizzards (rs : cs) (x, y + 1)
  where
    maybeAddBlizzard (Bliz d) p l = Blizzard d p : l
    maybeAddBlizzard _ _ l = l

outOfBounds :: Size -> Position -> Bool
outOfBounds (r, c) (x, y) = x < 0 || y < 0 || x >= r || y >= c

updateBlizzard :: Size -> Blizzard -> Blizzard
updateBlizzard (r, c) (Blizzard dir pos) =
  let pos' = move dir pos
      pos'' = if outOfBounds (r, c) pos' then newBlizzard dir pos' else pos'
   in Blizzard dir pos''
  where
    newBlizzard U (-1, y) = (r - 1, y)
    newBlizzard D (_, y) = (0, y)
    newBlizzard L (x, -1) = (x, c - 1)
    newBlizzard R (x, _) = (x, 0)
    newBlizzard d p = error ("Invalid Blizzard " ++ show (Blizzard d p))

move :: Direction -> Position -> Position
move U = first (subtract 1)
move D = first (+ 1)
move L = second (subtract 1)
move R = second (+ 1)

updatePlayer :: Grid -> [Blizzard] -> Position -> [Position]
updatePlayer (Grid gridSize start end) blizzards pos = filter isAllowedPos (pos : (move <$> [U, D, L, R] <*> [pos]))
  where
    blizPos = S.fromList $ map getBlizzardPos blizzards
    isAllowedPos p = (not (outOfBounds gridSize p) || p == start || p == end) && p `notMember` blizPos

updateGridState :: Grid -> GridState -> GridState
updateGridState grid (GridState blizzards pos) =
  let blizzards' = map (updateBlizzard (getGridSize grid)) blizzards
      pos' = concatMap (updatePlayer grid blizzards') pos
   in GridState blizzards' (nub pos')

runSimulation :: Grid -> GridState -> [GridState]
runSimulation grid = tail . iterate (updateGridState grid)

goToGoal :: Goal -> Grid -> GridState -> [GridState]
goToGoal goal grid state = takeUntil hasArrived $ runSimulation grid state
  where
    hasArrived s = goalCoord grid goal `elem` getPossiblePos s

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (a : rs) = if f a then [a] else a : takeUntil f rs

goToGoals :: [Goal] -> Grid -> GridState -> [GridState]
goToGoals goals grid state = concat $ tail $ scanl goToGoalAndReset [state] goals
  where
    otherGoal Start = End
    otherGoal End = Start
    goToGoalAndReset states goal = goToGoal goal grid (resetState grid (otherGoal goal) (last states))

goalCoord :: Grid -> Goal -> Position
goalCoord grid Start = getStart grid
goalCoord grid End = getEnd grid

resetState :: Grid -> Goal -> GridState -> GridState
resetState grid goal (GridState blizzards _) = GridState blizzards [goalCoord grid goal]

-- Parser

parseTile :: Parser Tile
parseTile = (Open <$ char '.') <|> (Wall <$ char '#') <|> parseBlizzard

parseBlizzard :: Parser Tile
parseBlizzard = Bliz <$> ((L <$ char '<') <|> (R <$ char '>') <|> (U <$ char '^') <|> (D <$ char 'v'))

parseInput :: Parser [[Tile]]
parseInput = (many1 parseTile `sepEndBy1` newline) <* eof

part1 :: [[Tile]] -> IO ()
part1 = print . length . uncurry (goToGoal End) . initialState

part2 :: [[Tile]] -> IO ()
part2 = print . length . uncurry (goToGoals [End, Start, End]) . initialState
