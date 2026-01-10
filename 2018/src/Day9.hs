module Day9
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Sequence
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Prelude hiding (drop, length, replicate, take)

playMarble :: Int -> Seq Int -> (Int, Seq Int)
playMarble marble circle
  | marble > 0 && (marble `mod` 23) == 0 =
      let i = length circle - 7
       in (marble + circle `index` i, drop (i + 1) circle >< take i circle)
  | otherwise = (0, (marble <| drop 2 circle) >< take 2 circle)

playUntil :: Int -> [Int]
playUntil lastMarble = go 0 Empty
  where
    go marble circle
      | marble > lastMarble = []
      | otherwise = let (score, circle') = playMarble marble circle in score : go (marble + 1) circle'

gameScores :: Int -> Int -> Seq Int
gameScores numPlayers lastMarble =
  foldl updateScore (replicate numPlayers 0) (playUntil lastMarble)

updateScore :: Seq Int -> Int -> Seq Int
updateScore scores score = case viewl scores of
  EmptyL -> Empty
  (a :< as) -> as |> (a + score)

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
part2 (numPlayers, lastMarble) = print $ maximum (gameScores numPlayers (lastMarble * 100))
