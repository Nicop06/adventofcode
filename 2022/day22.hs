import Data.Array.Repa (Array, DIM2, ix2, extent, (!))
import Data.Array.Repa.Shape (listOfShape)
import Data.Array.Repa.Repr.Vector (V, fromListVector)
import Control.Arrow (first, second)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

data Tile = None | Open | Wall deriving (Show, Eq)

type Grid = Array V DIM2 Tile;

data Instruction = NumberOftiles Int | TurnRight | TurnLeft deriving (Show, Eq)

data Direction = R | D | L | U deriving (Enum, Show, Eq)

type Path = [Instruction]

type Coordinates = (Int, Int)

type Player = (Coordinates, Direction)

-- Helpers

listToGrid :: [[Tile]] -> Grid
listToGrid tiles = let maxLen = maximum . map length $ tiles in
    fromListVector (ix2 (length tiles) maxLen) $ concat $ map (take maxLen . (++ repeat None)) $ tiles

initialCoordinate :: Grid -> Coordinates
initialCoordinate grid =
    let (_, numCols) = gridSize grid
        openTile = fst . head . filter ((==Open) . snd) . map (\c -> (c, grid ! ix2 0 c)) $ [0..(numCols - 1)]
        in (0, openTile)

gridSize :: Grid -> Coordinates
gridSize grid = let [c, r] = listOfShape . extent $ grid in (r, c)

initialPlayer :: Grid -> Player
initialPlayer grid = (initialCoordinate grid, R)

followInstruction :: Grid -> Instruction -> Player -> Player
followInstruction _ TurnRight = second turnRight
followInstruction _ TurnLeft = second turnLeft
followInstruction grid (NumberOftiles n) = move n grid

turnRight :: Direction -> Direction
turnRight U = R
turnRight d = succ d

turnLeft :: Direction -> Direction
turnLeft R = U
turnLeft d = pred d

wrapGrid :: Grid -> Coordinates -> Coordinates
wrapGrid grid (x, y) = let (r, c) = gridSize grid in (x `modulo` r, y `modulo` c)
    where modulo a b = let m = a `mod` b in if m < 0 then m + b else m

nextCoord :: Grid -> Direction -> Coordinates -> Coordinates
nextCoord grid U = first (subtract 1)
nextCoord grid D = first (+1)
nextCoord grid L = second (subtract 1)
nextCoord grid R = second (+1)

nextTile :: Grid -> Direction -> Coordinates -> Coordinates
nextTile grid dir coord = let (x, y) = wrapGrid grid $ nextCoord grid dir coord in case (grid ! ix2 x y) of
    None -> nextTile grid dir (x, y)
    otherwise -> (x, y)

move :: Int -> Grid -> Player -> Player
move 0 _ p = p
move steps grid (coord, dir) = let (x, y) = nextTile grid dir coord in case (grid ! ix2 x y) of
    Wall -> (coord, dir)
    otherwise -> move (steps - 1) grid ((x, y), dir)

followPath :: Grid -> Path -> [Player]
followPath grid = scanl (flip (followInstruction grid)) (initialPlayer grid)

finalPassword :: Grid -> Path -> Int
finalPassword grid path = let ((x, y), dir) = last $ followPath grid path in
    1000 * (x + 1) + 4 * (y + 1) + fromEnum dir

-- Parser

parseTile :: Parser Tile
parseTile = (Open <$ char '.') <|> (Wall <$ char '#') <|> (None <$ char ' ')

parseGrid :: Parser Grid
parseGrid = listToGrid <$> ((many1 parseTile) `sepEndBy1` newline) <* newline

parsePath :: Parser Path
parsePath = many1 ((NumberOftiles . read <$> many1 digit) <|> (TurnRight <$ char 'R') <|> (TurnLeft <$ char 'L')) <* newline

parseInput :: Parser (Grid, Path)
parseInput = (,) <$> parseGrid <*> parsePath <* eof

part1 :: Parser Int
part1 = uncurry finalPassword <$> parseInput

part2 :: Parser Int
part2 = return 0

main :: IO ()
main = parseAndSolve "inputs/day22" part1 part2
--main = parseAndSolve "/tmp/testinput" part1 part2
