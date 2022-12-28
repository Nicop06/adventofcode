import Control.Arrow (first, second)
import Data.List (nub)
import Data.Set qualified as S
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

data Tile = Open | Wall | Bliz Direction deriving (Show, Eq)

type Position = (Int, Int)

type Size = (Int, Int)

data Direction = U | D | L | R deriving (Show, Eq)

data Blizzard = Blizzard {blizzardDir :: Direction, blizzardPos :: Position} deriving (Show)

data GridState = GridState {blizzards :: [Blizzard], possiblePos :: [Position]} deriving (Show)

data Grid = Grid {gridSize :: Size, start :: Position, end :: Position}

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
initialBlizzards ([] : cs) (x, y) = initialBlizzards cs (x + 1, 0)
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
    newBlizzard D (r, y) = (0, y)
    newBlizzard L (x, -1) = (x, c - 1)
    newBlizzard R (x, c) = (x, 0)
    newBlizzard d p = error ("Invalid Blizzard " ++ show (Blizzard d p))

move :: Direction -> Position -> Position
move U = first (subtract 1)
move D = first (+ 1)
move L = second (subtract 1)
move R = second (+ 1)

updatePlayer :: Grid -> [Blizzard] -> Position -> [Position]
updatePlayer (Grid gridSize start end) blizzards pos = filter isAllowedPos (pos : (move <$> [U, D, L, R] <*> [pos]))
  where
    blizPos = S.fromList $ map blizzardPos blizzards
    isAllowedPos pos = (not (outOfBounds gridSize pos) || pos == start || pos == end) && pos `S.notMember` blizPos

updateGridState :: Grid -> GridState -> GridState
updateGridState grid (GridState blizzards pos) =
  let blizzards' = map (updateBlizzard (gridSize grid)) blizzards
      pos' = concatMap (updatePlayer grid blizzards') pos
   in GridState blizzards' (nub pos')

runSimulation :: Grid -> GridState -> [GridState]
runSimulation grid = tail . iterate (updateGridState grid)

goToGoal :: Goal -> Grid -> GridState -> [GridState]
goToGoal goal grid state = takeUntil hasArrived $ runSimulation grid state
  where
    hasArrived state = goalCoord grid goal `elem` possiblePos state

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f [] = []
takeUntil f (a : rs) = if f a then [a] else a : takeUntil f rs

goToGoals :: [Goal] -> Grid -> GridState -> [GridState]
goToGoals goals grid state = concat $ tail $ scanl goToGoalAndReset [state] goals
  where
    otherGoal Start = End
    otherGoal End = Start
    goToGoalAndReset states goal = goToGoal goal grid (resetState grid (otherGoal goal) (last states))

goalCoord :: Grid -> Goal -> Position
goalCoord grid Start = start grid
goalCoord grid End = end grid

resetState :: Grid -> Goal -> GridState -> GridState
resetState grid goal (GridState blizzards _) = GridState blizzards [goalCoord grid goal]

-- Debug

printGrid :: Grid -> GridState -> [String]
printGrid (Grid (r, c) start end) (GridState bliz pos) =
  top : ['#' : [printChar (x, y) | y <- [0 .. (c - 1)]] ++ "#" | x <- [0 .. (r - 1)]] ++ [bot]
  where
    top = (if start `elem` pos then "#E" else "#.") ++ replicate c '#'
    bot = replicate c '#' ++ if end `elem` pos then "E#" else ".#"
    printChar p
      | p `elem` pos = 'E'
      | otherwise = case filter ((== p) . blizzardPos) bliz of
          [] -> '.'
          [b] -> printDirection . blizzardDir $ b
          lb -> head $ show (length lb)
    printDirection U = '^'
    printDirection D = 'v'
    printDirection L = '<'
    printDirection R = '>'

-- Parser

parseTile :: Parser Tile
parseTile = (Open <$ char '.') <|> (Wall <$ char '#') <|> parseBlizzard

parseBlizzard :: Parser Tile
parseBlizzard = Bliz <$> ((L <$ char '<') <|> (R <$ char '>') <|> (U <$ char '^') <|> (D <$ char 'v'))

parseGrid :: Parser [[Tile]]
parseGrid = (many1 parseTile `sepEndBy1` newline) <* eof

part1 :: Parser Int
part1 = length . uncurry (goToGoal End) . initialState <$> parseGrid

part2 :: Parser Int
part2 = length . uncurry (goToGoals [End, Start, End]) . initialState <$> parseGrid

debugState :: IO ()
debugState = do
  Right (grid, state) <- parseFromFile (initialState <$> parseGrid) "/tmp/testinput"
  mapM_ putStrLn $ concatMap (printGrid grid) $ goToGoals [End, Start, End] grid state

main :: IO ()
main = parseAndSolve "inputs/day24" part1 part2
