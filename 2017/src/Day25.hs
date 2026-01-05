module Day25 (
  parseInput,
  part1,
  part2,
)
where

import Control.Arrow
import Data.Map as M (Map, fromList, lookup)
import Data.IntSet as S (IntSet, insert, empty, size, member, delete)
import Text.Parsec
import Text.Parsec.String

type Tape = IntSet

data Direction = L | R deriving (Show)

data Machine = Machine {stage :: Char, position :: Int, tape :: Tape} deriving (Show)

data Instruction = Instruction {valueToWrite :: Int, direction :: Direction, nextState :: Char} deriving (Show)

type Program = Map (Char, Int) Instruction

data Puzzle = Puzzle {startingStage :: Char, numStep :: Int, instructions :: Program} deriving (Show)

move :: Direction -> Int -> Int
move L = subtract 1
move R = (+ 1)

setValue :: Int -> Int -> Tape -> Tape
setValue 0 = S.delete
setValue 1 = S.insert
setValue v = error $ "Invalid value." ++ show v

executeInstruction :: Program -> Machine -> Machine
executeInstruction program (Machine s p t) =
  let v = if member p t then 1 else 0
   in case M.lookup (s , v) program of
        Nothing -> error $ "Cannot find instruction for state " ++ show s ++ " and tape value " ++ show v
        Just (Instruction newVal d newState) -> Machine newState (move d p) (setValue newVal p t)

parseInitStage :: Parser Char
parseInitStage = between (string "Begin in state ") (string ".\n") anyChar

parseNumSteps :: Parser Int
parseNumSteps = between (string "Perform a diagnostic checksum after ") (string " steps.\n\n") (read <$> many1 digit)

parseInstruction :: Parser Instruction
parseInstruction = Instruction <$> parseValueToWrite <*> parseDirection <*> parseNextState
 where
  parseValueToWrite :: Parser Int
  parseValueToWrite = between (string "    - Write the value ") (string ".\n") (read . pure <$> oneOf "01")

  parseDirection :: Parser Direction
  parseDirection = between (string "    - Move one slot to the ") (string ".\n") ((R <$ string "right") <|> (L <$ string "left"))

  parseNextState :: Parser Char
  parseNextState = between (string "    - Continue with state ") (string ".\n") anyChar

parseStateInstructions :: Parser [((Char, Int), Instruction)]
parseStateInstructions =
  makeInstructions
    <$> parseStateStage
    <*> count 2 ((,) <$> parseTapeValue <*> parseInstruction)
 where
  parseStateStage :: Parser Char
  parseStateStage = between (string "In state ") (string ":\n") anyChar

  parseTapeValue :: Parser Int
  parseTapeValue = between (string "  If the current value is ") (string ":\n") (read . pure <$> oneOf "01")

  makeInstructions :: Char -> [(Int, Instruction)] -> [((Char, Int), Instruction)]
  makeInstructions c = map (first (c ,))

parseInput :: Parser Puzzle
parseInput =
  Puzzle
    <$> parseInitStage
    <*> parseNumSteps
    <*> (fromList . concat <$> (parseStateInstructions `sepEndBy1` newline))
    <* eof

part1 :: Puzzle -> IO ()
part1 (Puzzle s n prog) = print $ checksum (iterate (executeInstruction prog) (Machine s 0 S.empty) !! n)
  where
    checksum :: Machine -> Int
    checksum (Machine _ _ t) = size t

part2 :: Puzzle -> IO ()
part2 = putStrLn . const "We are done!!"
