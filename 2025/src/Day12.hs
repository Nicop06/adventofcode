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

data ShapePart = Filled | Free deriving (Show, Eq)

type Present = Array (Int, Int) ShapePart

data Region = Region {getWidth :: Int, getHeight :: Int, getNumPresents :: [Int]} deriving (Show)

data Puzzle = Puzzle {getPresents :: [Present], getRegions :: [Region]} deriving (Show)

parseShapePart :: Parser ShapePart
parseShapePart = Filled <$ char '#' <|> Free <$ char '.'

presentSize :: Present -> Int
presentSize = length . filter (== Filled) . elems

canFit :: [Present] -> Region -> Bool
canFit presents (Region w h numPresents) = sum (zipWith (*) (map presentSize presents) numPresents) <= w * h

parsePresent :: Parser Present
parsePresent = between parseHeader newline parseShape
  where
    parseHeader :: Parser ()
    parseHeader = void $ digit <* char ':' <* newline
    parseShape :: Parser Present
    parseShape = listArray ((0, 0), (2, 2)) . concat <$> count 3 (count 3 parseShapePart <* newline)

parseRegion :: Parser Region
parseRegion = Region <$> readNumber <* char 'x' <*> readNumber <* string ": " <*> readNumber `sepBy1` char ' '
  where
    readNumber :: Parser Int
    readNumber = read <$> many1 digit

parseInput :: Parser Puzzle
parseInput = Puzzle <$> many1 (try parsePresent) <*> (parseRegion `sepEndBy1` newline) <* eof

part1 :: Puzzle -> IO ()
part1 (Puzzle presents regions) = print . length . filter (canFit presents) $ regions

part2 :: Puzzle -> IO ()
part2 = print . const 1
