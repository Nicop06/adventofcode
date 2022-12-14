module Day14
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (transpose)
import Text.Parsec
import Text.Parsec.String

-- Data

data Reindeer = Reindeer {name :: String, speed :: Int, flyTime :: Int, restTime :: Int} deriving (Show)

-- Helpers

distancePerSecond :: Reindeer -> [Int]
distancePerSecond r = scanl1 (+) (concat . repeat $ fly ++ rest)
  where
    fly = replicate (flyTime r) (speed r)
    rest = replicate (restTime r) 0

reindeerScore :: Int -> [Reindeer] -> [Int]
reindeerScore time = map sum . transpose . map computeScore . transpose . map (take time . distancePerSecond)
  where
    computeScore dist =
      let maxDist = maximum dist
       in map (\d -> if d == maxDist then 1 else 0) dist

-- Parser

parseName :: Parser String
parseName = many1 alphaNum

parseSpeed :: Parser Int
parseSpeed = read <$> many1 digit <* string " km/s"

parseTime :: Parser Int
parseTime = read <$> many1 digit <* string " seconds"

parseReindeer :: Parser Reindeer
parseReindeer = Reindeer <$> parseName <*> (string " can fly " *> parseSpeed) <*> (string " for " *> parseTime) <*> (string ", but then must rest for " *> parseTime <* char '.')

parseInput :: Parser [Reindeer]
parseInput = parseReindeer `sepEndBy1` newline <* eof

part1 :: [Reindeer] -> IO ()
part1 = print . maximum . map ((!! 2503) . distancePerSecond)

part2 :: [Reindeer] -> IO ()
part2 = print . maximum . reindeerScore 2503
