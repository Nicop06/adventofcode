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

parseRow :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m [a]
parseRow parser = parser `sepEndBy1` skipMany1 (char ' ')

parseNumbersRow :: Parser [Int]
parseNumbersRow = parseRow (read <$> many1 digit)

parseOperation :: Parser Operation
parseOperation = (Add <$ char '+') <|> (Multiply <$ char '*')

parseOperationRow :: Parser [Operation]
parseOperationRow = parseRow parseOperation

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
data OperationOrSpace
  = Operation Operation
  | Space

data NumberOrNone
  = Number Int
  | NaN

data Part2 =
  Part2 [[NumberOrNone]] [OperationOrSpace]

solvePart2 :: Part2 -> Int
solvePart2 (Part2 numbers operations) = length numbers + length operations

parseNumberOrNone :: Parser NumberOrNone
parseNumberOrNone = (Number . read . pure <$> digit) <|> (NaN <$ char ' ')

parseOperationOrSpace :: Parser OperationOrSpace
parseOperationOrSpace = (Operation <$> parseOperation) <|> (Space <$ char ' ')

parsePart2 :: Parser Part2
parsePart2 =
  Part2 <$> (many1 parseNumberOrNone `sepEndBy1` newline) <*>
  many1 parseOperationOrSpace <*
  newline <*
  eof

part2 :: FilePath -> IO ()
part2 file = parseFromFile parsePart2 file >>= either print (print . solvePart2)
