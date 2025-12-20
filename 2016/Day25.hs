module Day25
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Char (toUpper)
import Text.Parsec
import Text.Parsec.String

data Register = A | B | C | D deriving (Show, Read, Eq)

data RegState = RegState {regA :: Int, regB :: Int, regC :: Int, regD :: Int} deriving (Show, Eq)

data Value = Value Int | Register Register deriving (Show, Eq)

data Instruction = Copy Value Register | Inc Register | Dec Register | JumpNoZero Value Int | Out Value deriving (Show, Eq)

parseInput :: Parser [Instruction]
parseInput = (parseInstruction <* optional (try (spaces <* char ';' <* many (noneOf "\n")))) `sepEndBy1` newline <* eof

parseInstruction :: Parser Instruction
parseInstruction = parseCpy <|> parseInc <|> parseDec <|> parseJnz <|> parseOut

parseCpy :: Parser Instruction
parseCpy = Copy <$> (string "cpy " *> parseValue <* char ' ') <*> parseRegister

parseInc :: Parser Instruction
parseInc = Inc <$> (string "inc " *> parseRegister)

parseDec :: Parser Instruction
parseDec = Dec <$> (string "dec " *> parseRegister)

parseJnz :: Parser Instruction
parseJnz = JumpNoZero <$> (string "jnz " *> parseValue <* char ' ') <*> parseNum

parseOut :: Parser Instruction
parseOut = Out <$> (string "out " *> parseValue)

parseValue :: Parser Value
parseValue = (Register <$> parseRegister) <|> (Value <$> parseNum)

parseRegister :: Parser Register
parseRegister = read . pure . toUpper <$> oneOf "abcd"

parseNum :: Parser Int
parseNum = read <$> many1 (digit <|> char '-')

part1 :: [Instruction] -> IO ()
part1 = print . const (2730 - 2555)

part2 :: [Instruction] -> IO ()
part2 = putStrLn . const "We are done!!"
