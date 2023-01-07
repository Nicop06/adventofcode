module Day2() where
import Data.List (find)
import Data.Maybe (fromJust)
import ParseAndRun

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

part1 :: [String] -> Int
part1 = totalScore . map readRoundPart1

part2 :: [String] -> Int
part2 = totalScore . map readRoundPart2

--main :: IO ()
--main = parseAndRun "inputs/day2" part1 part2
