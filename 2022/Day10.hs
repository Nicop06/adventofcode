module Day10 (parseInput,part1
,part2) where

import Text.Parsec
import Text.Parsec.String

-- Data

cyclesToAdd :: [Int]
cyclesToAdd = [20, 60, 100, 140, 180, 220]

initialValue :: Int
initialValue = 1

-- Helpers

executeCycles :: [Int -> Int] -> [Int]
executeCycles = init . scanl (flip ($)) initialValue

strengthPerCycles :: [Int] -> [Int]
strengthPerCycles = zipWith (*) [1 ..]

sumStrength :: [Int] -> Int
sumStrength l = sum [l !! (i - 1) | i <- cyclesToAdd]

drawPixel :: Int -> Int -> String
drawPixel c sprite
  | abs (col - sprite) <= 1 = '#' : newRow
  | otherwise = '.' : newRow
  where
    col = c `mod` 40
    newRow = if col == 39 then ['\n'] else ""

drawScreen :: [Int] -> String
drawScreen = concat . zipWith drawPixel [0 ..]

-- Parser

noop :: Parser [Int -> Int]
noop = [id] <$ string "noop" <* newline

addx :: Parser [Int -> Int]
addx = (\x -> [id, (+ read x)]) <$> (string "addx " *> many1 (digit <|> char '-') <* newline)

parseInput :: Parser [Int -> Int]
parseInput = concat <$> many1 (addx <|> noop) <* eof

part1 :: [Int -> Int] -> IO ()
part1 = print . sumStrength . strengthPerCycles . executeCycles

part2 :: [Int -> Int] -> IO ()
part2 = mapM_ putStrLn . lines . drawScreen . executeCycles
