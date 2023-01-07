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

elfShapeFromCode :: String -> Maybe Shape
elfShapeFromCode "A" = Just Rock
elfShapeFromCode "B" = Just Paper
elfShapeFromCode "C" = Just Scissor
elfShapeFromCode _ = Nothing

playerShapeFromCode :: String -> Maybe Shape
playerShapeFromCode "X" = Just Rock
playerShapeFromCode "Y" = Just Paper
playerShapeFromCode "Z" = Just Scissor
playerShapeFromCode _ = Nothing

outcomeFromCode :: String -> Maybe Outcome
outcomeFromCode "X" = Just Lose
outcomeFromCode "Y" = Just Draw
outcomeFromCode "Z" = Just Win
outcomeFromCode _ = Nothing

roundScore :: Round -> Int
roundScore (Round elfShape myShape) = outcomeScore outcome + shapeScore myShape
  where
    outcome = roundOutcome elfShape myShape

totalScore :: [Round] -> Int
totalScore = sum . map roundScore

readRoundPart1 :: String -> Round
readRoundPart1 roundLine =
  let roundWords = words roundLine
      elfShape = fromJust $ elfShapeFromCode $ head roundWords
      playerShape = fromJust $ playerShapeFromCode (roundWords !! 1)
   in Round elfShape playerShape

readRoundPart2 :: String -> Round
readRoundPart2 roundLine =
  let roundWords = words roundLine
      elfShape = fromJust $ elfShapeFromCode $ head roundWords
      outcome = fromJust $ outcomeFromCode (roundWords !! 1)
   in Round elfShape (shapeForOutcome elfShape outcome)

parseInput :: Parser [String]
parseInput = lines <$> many1 anyChar

part1 :: [String] -> IO ()
part1 = print . totalScore . map readRoundPart1

part2 :: [String] -> IO ()
part2 = print . totalScore . map readRoundPart2
