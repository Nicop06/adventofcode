module Day22
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (first, second)
import Data.Array.Repa (Array, DIM2, extent, ix2, (!))
import Data.Array.Repa.Repr.Vector (V, fromListVector)
import Data.Array.Repa.Shape (listOfShape)
import Text.Parsec
import Text.Parsec.String

-- Data

data Tile = None | Open | Wall deriving (Show, Eq)

type Grid = Array V DIM2 Tile

data Instruction = NumberOftiles Int | TurnRight | TurnLeft deriving (Show, Eq)

data Direction = R | D | L | U deriving (Enum, Show, Eq)

type Path = [Instruction]

type Coordinates = (Int, Int)

type GridSize = (Int, Int)

type Player = (Coordinates, Direction)

type NextTileFunc = Grid -> Player -> Player

-- Helpers

listToGrid :: [[Tile]] -> Grid
listToGrid tiles =
  let maxLen = maximum . map length $ tiles
   in fromListVector (ix2 (length tiles) maxLen) $ concatMap (take maxLen . (++ repeat None)) tiles

initialCoordinate :: Grid -> Coordinates
initialCoordinate grid =
  let (_, numCols) = gridSize grid
      openTile = fst . head . filter ((== Open) . snd) . map (\c -> (c, grid ! ix2 0 c)) $ [0 .. (numCols - 1)]
   in (0, openTile)

gridSize :: Grid -> GridSize
gridSize grid = case listOfShape . extent $ grid of
  [c, r] -> (r, c)
  _ -> error "Invalid dimension for grid"

initialPlayer :: Grid -> Player
initialPlayer grid = (initialCoordinate grid, R)

followInstruction :: NextTileFunc -> Grid -> Instruction -> Player -> Player
followInstruction _ _ TurnRight = second turnRight
followInstruction _ _ TurnLeft = second turnLeft
followInstruction nextTileFunc grid (NumberOftiles n) = move n nextTileFunc grid

turnRight :: Direction -> Direction
turnRight U = R
turnRight d = succ d

turnLeft :: Direction -> Direction
turnLeft R = U
turnLeft d = pred d

wrapGrid :: Grid -> Coordinates -> Coordinates
wrapGrid grid (x, y) = let (r, c) = gridSize grid in (x `modulo` r, y `modulo` c)
  where
    modulo a b = let m = a `mod` b in if m < 0 then m + b else m

nextCoord :: Direction -> Coordinates -> Coordinates
nextCoord U = first (subtract 1)
nextCoord D = first (+ 1)
nextCoord L = second (subtract 1)
nextCoord R = second (+ 1)

nextTile :: Grid -> Player -> Player
nextTile grid (coord, dir) =
  let (x, y) = wrapGrid grid $ nextCoord dir coord
   in case grid ! ix2 x y of
        None -> nextTile grid ((x, y), dir)
        _ -> ((x, y), dir)

move :: Int -> NextTileFunc -> Grid -> Player -> Player
move 0 _ _ p = p
move steps nextTileFunc grid player =
  let ((x, y), dir') = nextTileFunc grid player
   in case grid ! ix2 x y of
        Wall -> player
        _ -> move (steps - 1) nextTileFunc grid ((x, y), dir')

followPath :: NextTileFunc -> Grid -> Path -> [Player]
followPath nextTileFunc grid = scanl (flip (followInstruction nextTileFunc grid)) (initialPlayer grid)

password :: Player -> Int
password ((x, y), dir) = 1000 * (x + 1) + 4 * (y + 1) + fromEnum dir

-- Cube mapping

{-
Cube shape:
   UR
   F
  LD
  B

With U = up, R = right, F = forward, D = down, L = left, B = bottom.

Missing link:
U -> B
B -> U
U -> L
L -> U
R -> B
B -> R
R -> D
D -> R
R -> F
F -> R
F -> L
L -> F
D -> B
B -> D
-}

nextTileCube :: Grid -> Player -> Player
nextTileCube grid p@((x, y), dir) =
  let (x', y') = nextCoord dir (x, y)
   in if isOnTile grid (x', y')
        then ((x', y'), dir)
        else wrapCube
  where
    wrapCube
      | x == 0 && y >= 50 && y < 100 && dir == U = ((y + 100, 0), R) -- U -> B
      | x >= 150 && x < 200 && y == 0 && dir == L = ((0, x - 100), D) -- B -> U
      | x >= 0 && x < 50 && y == 50 && dir == L = ((149 - x, 0), R) -- U -> L
      | x >= 100 && x < 150 && y == 0 && dir == L = ((149 - x, 50), R) -- L -> U
      | x == 0 && y >= 100 && y < 150 && dir == U = ((199, y - 100), U) -- R -> B
      | x == 199 && y >= 0 && y < 50 && dir == D = ((0, y + 100), D) -- B -> R
      | x >= 0 && x < 50 && y == 149 && dir == R = ((149 - x, 99), L) -- R -> D
      | x >= 100 && x < 150 && y == 99 && dir == R = ((149 - x, 149), L) -- D -> R
      | x == 49 && y >= 100 && y < 150 && dir == D = ((y - 50, 99), L) -- R -> F
      | x >= 50 && x < 100 && y == 99 && dir == R = ((49, x + 50), U) -- F -> R
      | x >= 50 && x < 100 && y == 50 && dir == L = ((100, x - 50), D) -- F -> L
      | x == 100 && y >= 0 && y < 50 && dir == U = ((y + 50, 50), R) -- L -> F
      | x == 149 && y >= 50 && y < 100 && dir == D = ((y + 100, 49), L) -- D -> B
      | x >= 150 && x < 200 && y == 49 && dir == R = ((149, x - 100), U) -- B -> D
      | otherwise = error ("Invalid position: " ++ show p)

isOnTile :: Grid -> Coordinates -> Bool
isOnTile grid (x, y) = let (r, c) = gridSize grid in x >= 0 && x < r && y >= 0 && y < c && (grid ! ix2 x y) /= None

-- Parser

parseTile :: Parser Tile
parseTile = (Open <$ char '.') <|> (Wall <$ char '#') <|> (None <$ char ' ')

parseGrid :: Parser Grid
parseGrid = listToGrid <$> (many1 parseTile `sepEndBy1` newline) <* newline

parsePath :: Parser Path
parsePath = many1 ((NumberOftiles . read <$> many1 digit) <|> (TurnRight <$ char 'R') <|> (TurnLeft <$ char 'L')) <* newline

parseInput :: Parser (Grid, Path)
parseInput = (,) <$> parseGrid <*> parsePath <* eof

part1 :: (Grid, Path) -> IO ()
part1 = print . password . last . uncurry (followPath nextTile)

part2 :: (Grid, Path) -> IO ()
part2 = print . password . last . uncurry (followPath nextTileCube)
