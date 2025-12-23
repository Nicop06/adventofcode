module Day8
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Map as M (Map, elems, empty, findWithDefault, insertWith)
import Text.Parsec
import Text.Parsec.String

data Operation = Inc Int | Dec Int deriving (Show, Eq)

type Comparison = (Int -> Int -> Bool)

data Condition = Condition String Comparison Int

data Instruction = Instruction String Operation Condition

type RegState = Map String Int

getReg :: String -> RegState -> Int
getReg = findWithDefault 0

testCondition :: Condition -> RegState -> Bool
testCondition (Condition reg comp val) state = getReg reg state `comp` val

applyOp :: Operation -> String -> RegState -> RegState
applyOp (Inc n) reg = insertWith (+) reg n
applyOp (Dec n) reg = insertWith (+) reg (-n)

executeInstruction :: RegState -> Instruction -> RegState
executeInstruction regState (Instruction reg op cond)
  | testCondition cond regState = applyOp op reg regState
  | otherwise = regState

executeProgram :: [Instruction] -> [Map String Int]
executeProgram = scanl executeInstruction M.empty

parseInstruction :: Parser Instruction
parseInstruction =
  Instruction
    <$> many1 alphaNum
    <* char ' '
    <*> parseOperation
    <* char ' '
    <*> parseCondition
  where
    parseOperation :: Parser Operation
    parseOperation =
      ((Inc <$ string "inc ") <|> (Dec <$ string "dec "))
        <*> parseInt

    parseCondition :: Parser Condition
    parseCondition =
      (Condition <$ string "if ")
        <*> (many1 alphaNum <* char ' ')
        <*> (parseComparison <* char ' ')
        <*> parseInt

    parseComparison :: Parser Comparison
    parseComparison =
      try ((<=) <$ string "<=")
        <|> try ((>=) <$ string ">=")
        <|> (<) <$ string "<"
        <|> ((>) <$ string ">")
        <|> ((==) <$ string "==")
        <|> ((/=) <$ string "!=")

    parseInt :: Parser Int
    parseInt = read <$> many1 (digit <|> char '-')

parseInput :: Parser [Instruction]
parseInput = parseInstruction `sepEndBy1` newline <* eof

part1 :: [Instruction] -> IO ()
part1 = print . maximum . elems . last . executeProgram

part2 :: [Instruction] -> IO ()
part2 = print . maximum . concatMap elems . executeProgram
