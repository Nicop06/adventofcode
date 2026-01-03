module Day19
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.Array
import Text.Parsec
import Text.Parsec.String
import Prelude hiding (Left, Right)

data Tile = Space | Vertical | Horizontal | Corner | Letter Char deriving (Show)

type Coord = (Int, Int)

type Diagram = Array Coord Tile

data Direction = Up | Down | Left | Right deriving (Show, Eq, Bounded, Enum)

move :: Direction -> Coord -> Coord
move Up = first (subtract 1)
move Down = first (+ 1)
move Left = second (+ 1)
move Right = second (subtract 1)

turn :: Direction -> [Direction]
turn Up = [Left, Right]
turn Down = [Left, Right]
turn Left = [Up, Down]
turn Right = [Up, Down]

isSpace :: Tile -> Bool
isSpace Space = True
isSpace _ = False

fromList :: [[Tile]] -> Diagram
fromList l =
  let w = length (head l)
      h = length l
   in listArray ((1, 1), (h, w)) (concat l)

followPath :: Diagram -> [Tile]
followPath diagram = go Down firstCoord
  where
    firstCoord :: Coord
    firstCoord = fst . head . filter (not . isSpace . snd) $ assocs diagram

    isPath :: Coord -> Bool
    isPath c = inRange (bounds diagram) c && not (isSpace (diagram ! c))

    neighbours :: Direction -> Coord -> [(Direction, Coord)]
    neighbours d c = filter (isPath . snd) $ map (id &&& flip move c) (turn d)

    go :: Direction -> Coord -> [Tile]
    go d c = case diagram ! c of
      Space -> []
      Corner -> case neighbours d c of
        [(d', c')] -> Corner : go d' c'
        cs -> error $ "More than one direction from " ++ show c ++ " going " ++ show d ++ ": " ++ show cs
      tile -> tile : go d (move d c)

parseTile :: Parser Tile
parseTile =
  (Vertical <$ char '|')
    <|> (Horizontal <$ char '-')
    <|> (Corner <$ char '+')
    <|> (Space <$ char ' ')
    <|> (Letter <$> alphaNum)

parseInput :: Parser Diagram
parseInput = fromList <$> (many1 parseTile `sepEndBy1` newline <* eof)

part1 :: Diagram -> IO ()
part1 = putStrLn . concatMap getLetter . followPath
  where
    getLetter :: Tile -> String
    getLetter (Letter t) = [t]
    getLetter _ = []

part2 :: Diagram -> IO ()
part2 = print . length . followPath
