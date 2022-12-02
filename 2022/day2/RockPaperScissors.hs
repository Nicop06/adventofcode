module RockPaperScissors
( Shape(..)
, Outcome(..)
, Round(..)
, elfShapeFromCode
, playerShapeFromCode
, totalScore
) where

data Shape = Rock | Paper | Scissor deriving (Eq, Show)

data Outcome = Lose | Draw | Win deriving (Eq, Show)

data Round = Round {elfShape :: Shape, myShape :: Shape}

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

roundScore :: Round -> Int
roundScore (Round elfShape myShape) = outcomeScore outcome + shapeScore myShape
  where
    outcome = roundOutcome elfShape myShape

totalScore :: [Round] -> Int
totalScore = sum . map roundScore
