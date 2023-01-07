module Day9(parseInput,part1
,part2) where
import Data.Bifunctor
import Data.List (nub)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type KnotPosition = (Int, Int)

type Rope = [KnotPosition]

data Direction = U | D | R | L deriving (Show, Read)

-- Helpers

isTouching :: KnotPosition -> KnotPosition -> Bool
isTouching h t = abs (fst h - fst t) <= 1 && abs (snd h - snd t) <= 1

isSameRow :: KnotPosition -> KnotPosition -> Bool
isSameRow h t = fst h == fst t

isSameCol :: KnotPosition -> KnotPosition -> Bool
isSameCol h t = snd h == snd t

moveTowards :: Int -> Int -> Int
moveTowards x y = x + signum (y - x)

moveTail :: KnotPosition -> KnotPosition -> KnotPosition
moveTail h t
  | isTouching h t = t
  | isSameRow h t = (fst t, moveTowards (snd t) (snd h))
  | isSameCol h t = (moveTowards (fst t) (fst h), snd t)
  | otherwise = bimap (moveTowards (fst t)) (moveTowards (snd t)) h

moveHead :: Direction -> KnotPosition -> KnotPosition
moveHead U (x, y) = (x, y + 1)
moveHead D (x, y) = (x, y - 1)
moveHead L (x, y) = (x - 1, y)
moveHead R (x, y) = (x + 1, y)

moveRope :: Direction -> Rope -> Rope
moveRope d r = scanl moveTail (moveHead d (head r)) (tail r)

executeMoves :: Int -> [Direction] -> [Rope]
executeMoves knots d = scanl (flip ($)) (replicate knots (0, 0)) (moveRope <$> d)

numTailPositions :: [Rope] -> Int
numTailPositions = length . nub . map last

-- Parser

direction :: Parser Direction
direction = read . (: []) <$> (char 'U' <|> char 'D' <|> char 'L' <|> char 'R')

moveCount :: Parser Int
moveCount = read <$> many1 digit

line :: Parser [Direction]
line = flip replicate <$> direction <* char ' ' <*> moveCount <* newline

parseInput :: Parser [Direction]
parseInput = concat <$> many1 line

part1 :: [Direction] -> IO ()
part1 = print . numTailPositions . executeMoves 2

part2 :: [Direction] -> IO ()
part2 = print . numTailPositions . executeMoves 10
