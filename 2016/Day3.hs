module Day3
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

data Triangle = Triangle Int Int Int deriving (Show, Eq)

isValid :: Triangle -> Bool
isValid (Triangle x y z) = x + y > z && x + z > y && y + z > x

parseInput :: Parser [[Int]]
parseInput = many1 parseTriangle <* eof
    where parseTriangle = Triangle <$> parseCoord <*> parseCoord <*> parseCoord <* newline
          parseCoord = read <$> (spaces *> many1 digit)

part1 :: [Triangle] -> IO ()
part1 = print . length . filter isValid

part2 :: [Triangle] -> IO ()
part2 = part1
