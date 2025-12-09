module Day9
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

type TileLocation = (Int, Int)

area :: TileLocation -> TileLocation -> Int
area (x, y) (x', y') = (abs (x - x') + 1) * (abs (y - y') + 1)

allPairs :: [a] -> [(a, a)]
allPairs (x : xs) = map (x,) xs ++ allPairs xs
allPairs [] = []

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseTileLocation :: Parser TileLocation
parseTileLocation = (,) <$> parseNumber <* char ',' <*> parseNumber

parseInput :: Parser [TileLocation]
parseInput = (parseTileLocation `sepEndBy1` newline) <* eof

part1 :: [TileLocation] -> IO ()
part1 = print . maximum . map (uncurry area) . allPairs

part2 :: [TileLocation] -> IO ()
part2 = print . length . map (uncurry area) . allPairs
