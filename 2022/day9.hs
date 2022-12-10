import Data.List
import Data.Bifunctor
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type KnotPosition = (Int, Int)

data Rope = Rope { getHead :: KnotPosition, getTail :: KnotPosition } deriving Show

data Direction = U | D | R | L deriving (Show, Read)

initialRope :: Rope
initialRope = Rope (0, 0) (0, 0)

-- Parser

isTouching :: KnotPosition -> KnotPosition -> Bool
isTouching h t = abs (fst h - fst t) <= 1 && abs (snd h - snd t) <= 1

isSameRow :: KnotPosition -> KnotPosition -> Bool
isSameRow h t = fst h == fst t

isSameCol :: KnotPosition -> KnotPosition -> Bool
isSameCol h t = snd h == snd t

moveTowards :: Int -> Int -> Int
moveTowards x y = x + signum (y - x)

moveTail :: Rope -> Rope
moveTail rope@(Rope h t)
    | isTouching h t = rope
    | isSameRow h t = Rope h (fst t, moveTowards (snd t) (snd h))
    | isSameCol h t = Rope h (moveTowards (fst t) (fst h), snd t)
    | otherwise = Rope h (bimap (moveTowards (fst t)) (moveTowards (snd t)) h)

moveHead :: Direction -> Rope -> Rope
moveHead U (Rope h t) = Rope (fst h, snd h + 1) t
moveHead D (Rope h t) = Rope (fst h, snd h - 1) t
moveHead L (Rope h t) = Rope (fst h - 1, snd h) t
moveHead R (Rope h t) = Rope (fst h + 1, snd h) t

moveHeadAndTail :: Direction -> Rope -> Rope
moveHeadAndTail d = moveTail . moveHead d

executeMoves :: [Direction] -> [Rope]
executeMoves d = scanl (flip ($)) initialRope (moveHeadAndTail <$> d)

numTailPositions :: [Rope] -> Int
numTailPositions = length . nub . map getTail

-- Parser

direction :: Parser Direction
direction = read . (:[]) <$> (char 'U' <|> char 'D' <|> char 'L' <|> char 'R')

moveCount :: Parser Int
moveCount = read <$> many1 digit

line :: Parser [Direction]
line = flip replicate <$> direction <* char ' ' <*> moveCount <* newline

input :: Parser [Direction]
input = concat <$> many1 line

part1 :: Parser Int
part1 = numTailPositions . executeMoves <$> input

part2 :: Parser Int
part2 = numTailPositions . executeMoves <$> input

main :: IO ()
main = parseAndSolve "inputs/day9" part1 part2
