{-# LANGUAGE FlexibleContexts #-}

module Day6
  ( part1
  , part2
  ) where

import Data.List (transpose)
import Text.Parsec
import Text.Parsec.String

data Operation
  = Add
  | Multiply
  deriving (Eq, Show)

data Part1 =
  Part1 [[Int]] [Operation]
  deriving (Eq, Show)

applyOperation :: Operation -> [Int] -> Int
applyOperation Add = sum
applyOperation Multiply = product

solvePart1 :: Part1 -> Int
solvePart1 (Part1 numbers operations) =
  sum $ zipWith applyOperation operations (transpose numbers)

parseRowWith :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m [a]
parseRowWith parser =
  skipMany (char ' ') *> parser `sepEndBy1` skipMany1 (char ' ')

parseNumbersRow :: Parser [Int]
parseNumbersRow = parseRowWith (read <$> many1 digit)

parseOperation :: Parser Operation
parseOperation = (Add <$ char '+') <|> (Multiply <$ char '*')

parseOperationRow :: Parser [Operation]
parseOperationRow = parseRowWith parseOperation

parsePart1 :: Parser Part1
parsePart1 =
  Part1 <$> (parseNumbersRow `sepEndBy1` newline) <*> parseOperationRow <*
  newline <*
  eof

part1 :: FilePath -> IO ()
part1 file = parseFromFile parsePart1 file >>= either print (print . solvePart1)

------------
-- Part 2 --
------------
data Part2 =
  Part2 [String] [Operation]
  deriving (Eq, Show)

groupProblems :: [String] -> [[String]]
groupProblems [] = []
groupProblems (s:rs)
  | all (== ' ') s = [] : groupProblems rs
  | otherwise =
    let groups = groupProblems rs
     in case groups of
          [] -> [[s]]
          (g:gs) -> (s : g) : gs

parseNumbers :: [String] -> [[Int]]
parseNumbers = map (map read) . groupProblems . transpose

solvePart2 :: Part2 -> Int
solvePart2 (Part2 numbers operations) =
  sum $ zipWith applyOperation operations (parseNumbers numbers)

parseRow :: Parser String
parseRow = many1 (digit <|> char ' ')

parsePart2 :: Parser Part2
parsePart2 =
  Part2 <$> (parseRow `sepEndBy1` newline) <*> parseOperationRow <* newline <*
  eof

part2 :: FilePath -> IO ()
part2 file = parseFromFile parsePart2 file >>= either print (print . solvePart2)
