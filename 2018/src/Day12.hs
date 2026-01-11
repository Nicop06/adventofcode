module Day12
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (find)
import Data.Maybe (isNothing)
import Text.Parsec
import Text.Parsec.String

type Pot = Bool

data Rule = Rule [Pot] Pot deriving (Show)

data Puzzle = Puzzle [Pot] [Rule] deriving (Show)

nextGeneration :: [Rule] -> [Pot] -> [Pot]
nextGeneration rules = go . (replicate 3 False ++)
  where
    applyRules :: [Rule] -> [Pot] -> Pot
    applyRules [] ps = ps !! 2
    applyRules ((Rule pattern result) : rs) ps
      | pattern == ps = result
      | otherwise = applyRules rs ps

    go :: [Pot] -> [Pot]
    go [] = []
    go ps = applyRules rules (take 5 (ps ++ repeat False)) : go (tail ps)

diff :: Num a => [a] -> [a]
diff (a1 : a2 : as) = a2 - a1 : diff (a2 : as)
diff _ = []

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x : xs) = isNothing $ find (x /=) xs

sumPotNumber :: Int -> [Pot] -> Int
sumPotNumber gen =
  sum
    . map fst
    . filter snd
    . zip [-gen ..]

runGeneration :: Int -> Puzzle -> Int
runGeneration numGen (Puzzle pots rules) =
  go 0 $ zipWith sumPotNumber [0 ..] $ iterate (nextGeneration rules) pots
  where
    go :: Int -> [Int] -> Int
    go gen (x1 : xs@(x2 : _))
      | gen == numGen = x1
      | allSame (take 10 (diff (x1 : xs))) = x1 + (numGen - gen) * (x2 - x1)
      | otherwise = go (gen + 1) xs
    go _ _ = 0

parsePot :: Parser Pot
parsePot = (True <$ char '#') <|> (False <$ char '.')

parseInitState :: Parser [Pot]
parseInitState =
  between
    (string "initial state: ")
    (count 2 newline)
    (many1 parsePot)

parseRule :: Parser Rule
parseRule =
  Rule
    <$> (count 5 parsePot <* string " => ")
    <*> parsePot

parseInput :: Parser Puzzle
parseInput =
  Puzzle
    <$> parseInitState
    <*> parseRule
    `sepEndBy1` newline
    <* eof

part1 :: Puzzle -> IO ()
part1 = print . runGeneration 20

part2 :: Puzzle -> IO ()
part2 = print . runGeneration 50000000000
