module Day3
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.Ix (range)
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

type Coord = (Int, Int)

data Direction = R | L | D | U deriving (Read, Show)

data Section = Section
  { _direction :: Direction,
    _steps :: Int
  }
  deriving (Show)

type Wire = [Section]

data Puzzle = Puzzle Wire Wire deriving (Show)

distance :: Coord -> Int
distance (x, y) = abs x + abs y

move :: Section -> Coord -> Coord
move (Section d s) = case d of
  U -> second (+ s)
  D -> second (subtract s)
  R -> first (+ s)
  L -> first (subtract s)

wirePath :: Wire -> M.Map Coord Int
wirePath = go 0 (0, 0)
  where
    makePath :: Coord -> Coord -> [Coord]
    makePath c c'
      | c < c' = range (c, c')
      | otherwise = reverse $ range (c', c)

    go :: Int -> Coord -> [Section] -> M.Map Coord Int
    go _ _ [] = M.empty
    go n c (s : ss) =
      let c' = move s c
       in M.unionWith
            min
            (M.fromList $ zip (makePath c c') [n .. n + _steps s])
            (go (n + _steps s) c' ss)

parseInput :: Parser Puzzle
parseInput = Puzzle <$> parseWire <*> parseWire <* eof
  where
    parseWire :: Parser Wire
    parseWire = parseSection `sepBy1` char ',' <* newline

    parseSection :: Parser Section
    parseSection = Section <$> parseDirection <*> parseInt

    parseDirection :: Parser Direction
    parseDirection = read . pure <$> oneOf "RLDU"

    parseInt :: Parser Int
    parseInt = read <$> many1 digit

part1 :: Puzzle -> IO ()
part1 (Puzzle w1 w2) =
  print
    . minimum
    . filter (/= 0)
    . map distance
    . M.keys
    $ crossings
  where
    crossings :: M.Map Coord Int
    crossings = M.intersection (wirePath w1) (wirePath w2)

part2 :: Puzzle -> IO ()
part2 (Puzzle w1 w2) =
  print
    . minimum
    . filter (/= 0)
    . M.elems
    $ crossings
  where
    crossings :: M.Map Coord Int
    crossings = M.intersectionWith (+) (wirePath w1) (wirePath w2)
