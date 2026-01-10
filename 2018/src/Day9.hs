module Day9
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Map as M (Map, fromListWith)
import Text.Parsec
import Text.Parsec.String

playMarble :: Int -> [Int] -> (Int, [Int])
playMarble marble circle
  | marble > 0 && (marble `mod` 23) == 0 =
      let i = length circle - 7
       in (marble + circle !! i, drop (i + 1) circle ++ take i circle)
  | otherwise = (0, marble : drop 2 circle ++ take 2 circle)

playUntil :: Int -> [Int]
playUntil lastMarble = go 0 []
  where
    go marble circle
      | marble > lastMarble = []
      | otherwise = let (score, circle') = playMarble marble circle in score : go (marble + 1) circle'

gameScores :: Int -> Int -> Map Int Int
gameScores numPlayers lastMarble =
  M.fromListWith (+) $ zip (cycle [1 .. numPlayers]) (playUntil lastMarble)

parseInput :: Parser (Int, Int)
parseInput = do
  numPlayers <- parseInt
  skipMany (noneOf ['0' .. '9'])
  lastMarble <- parseInt
  return (numPlayers, lastMarble)
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 digit

part1 :: (Int, Int) -> IO ()
part1 (numPlayers, lastMarble) = print $ maximum (gameScores numPlayers lastMarble)

part2 :: (Int, Int) -> IO ()
part2 _ = print $ maximum (gameScores 13 7999)
