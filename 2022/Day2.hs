module Day2
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

data Shape = Rock | Paper | Scissor deriving (Eq, Show)

data Outcome = Lose | Draw | Win deriving (Eq, Show)

data Round = Round Shape Shape

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissor = 3

outcomeScore :: Outcome -> Int
outcomeScore Lose = 0
outcomeScore Draw = 3
outcomeScore Win = 6

roundOutcome :: Shape -> Shape -> Outcome
roundOutcome Rock Paper = Win
roundOutcome Paper Scissor = Win
roundOutcome Scissor Rock = Win
roundOutcome elfShape playerShape
  | elfShape == playerShape = Draw
  | otherwise = Lose

shapeForOutcome :: Shape -> Outcome -> Shape
shapeForOutcome elfShape outcome = playerShape
  where
    playerShape = fromJust $ find (\ps -> roundOutcome elfShape ps == outcome) [Rock, Paper, Scissor]

roundScore :: Round -> Int
roundScore (Round elfShape myShape) = outcomeScore outcome + shapeScore myShape
  where
    outcome = roundOutcome elfShape myShape

totalScore :: [Round] -> Int
totalScore = sum . map roundScore

outcomeForPart2 :: Shape -> Outcome
outcomeForPart2 Rock = Lose
outcomeForPart2 Paper = Draw
outcomeForPart2 Scissor = Win

roundForPart2 :: Round -> Round
roundForPart2 (Round e p) = Round e (shapeForOutcome e $ outcomeForPart2 p)

parseRound :: Parser Round
parseRound = Round <$> (parseElfCode <* char ' ') <*> parsePlayerCode
    where parseElfCode = (Rock <$ char 'A') <|> (Paper <$ char 'B') <|> (Scissor <$ char 'C')
          parsePlayerCode = (Rock <$ char 'X') <|> (Paper <$ char 'Y') <|> (Scissor <$ char 'Z')

parseInput :: Parser [Round]
parseInput = parseRound `sepEndBy1` newline <* eof

part1 :: [Round] -> IO ()
part1 = print . totalScore

part2 :: [Round] -> IO ()
part2 = print . totalScore . map roundForPart2
