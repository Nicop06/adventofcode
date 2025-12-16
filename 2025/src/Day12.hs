module Day12
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Monad (void)
import Data.Array
import Text.Parsec
import Text.Parsec.String

data ShapePart = Filled | Free deriving (Show)

type Present = Array (Int, Int) ShapePart

data Region = Region {getWidth :: Int, getHeight :: Int, getNumPresents :: [Int]} deriving (Show)

data Puzzle = Puzzle {getPresents :: [Present], getRegions :: [Region]} deriving (Show)

parseShapePart :: Parser ShapePart
parseShapePart = Filled <$ char '#' <|> Free <$ char '.'

parsePresent :: Parser Present
parsePresent = between parseHeader newline parseShape
  where
    parseHeader :: Parser ()
    parseHeader = void $ digit <* char ':' <* newline
    parseShape :: Parser Present
    parseShape = listArray ((0, 0), (2, 2)) . concat <$> count 3 (count 3 parseShapePart <* newline)

parseRegion :: Parser Region
parseRegion = Region <$> readNumber <* char 'x' <*> readNumber <* string ": " <*> readNumber `sepBy1` char ' '
  where readNumber :: Parser Int
        readNumber = read <$> many1 digit

parseInput :: Parser Puzzle
parseInput = Puzzle <$> count 6 parsePresent <*> (parseRegion `sepEndBy1` newline) <* eof

part1 :: Puzzle -> IO ()
part1 = print

part2 :: Puzzle -> IO ()
part2 = print
