module Day25
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

type Point = (Int, Int, Int, Int)

type Constellation = [Point]

distance :: Point -> Point -> Int
distance (x, y, z, t) (x', y', z', t') =
  abs (x - x')
    + abs (y - y')
    + abs (z - z')
    + abs (t - t')

addToConstellations :: Point -> [Constellation] -> [Constellation]
addToConstellations p = go [p]
  where
    go :: Constellation -> [Constellation] -> [Constellation]
    go c [] = [c]
    go c (c' : cs)
      | any ((<= 3) . distance p) c' = go (c ++ c') cs
      | otherwise = c' : go c cs

parseInput :: Parser [Point]
parseInput = parsePoint `sepEndBy1` newline <* eof
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 (digit <|> char '-')

    parsePoint :: Parser Point
    parsePoint =
      (,,,)
        <$> parseInt
        <* char ','
        <*> parseInt
        <* char ','
        <*> parseInt
        <* char ','
        <*> parseInt

part1 :: [Point] -> IO ()
part1 = print . length . foldr addToConstellations []

part2 :: [Point] -> IO ()
part2 = putStrLn . const "We are done!!"
