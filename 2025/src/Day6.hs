{-# LANGUAGE FlexibleContexts #-}

module Day6
  ( parseInput
  , part1
  , part2
  ) where

import Data.List (transpose)
import Text.Parsec
import Text.Parsec.String

data Operation
  = Add
  | Multiply
  deriving (Eq, Show)

data Puzzle =
  Puzzle [[Int]] [Operation]
  deriving (Eq, Show)

applyOperation :: Operation -> [Int] -> Int
applyOperation Add = sum
applyOperation Multiply = product

solvePuzzle :: Puzzle -> Int
solvePuzzle (Puzzle numbers operations) =
  sum $ zipWith applyOperation operations (transpose numbers)

parseRow :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m [a]
parseRow parser = parser `sepEndBy1` skipMany (char ' ')

parseNumbersRow :: Parser [Int]
parseNumbersRow = parseRow (read <$> many1 digit)

parseOperation :: Parser Operation
parseOperation = (Add <$ char '+') <|> (Multiply <$ char '*')

parseOperationRow :: Parser [Operation]
parseOperationRow = parseRow parseOperation

parseInput :: Parser Puzzle
parseInput =
  Puzzle <$> (parseNumbersRow `sepEndBy1` newline) <*> parseOperationRow <*
  newline <*
  eof

part1 :: Puzzle -> IO ()
part1 = print . solvePuzzle

part2 :: Puzzle -> IO ()
part2 _ = print 1
