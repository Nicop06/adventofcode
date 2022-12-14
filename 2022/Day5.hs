module Day5
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Monad
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String

-- Data

type Crate = Char

type Stack = [Crate]

type NumCrates = Int

type From = Int

type To = Int

data Move = Move NumCrates From To deriving (Show)

-- Parser

integer :: Parser Int
integer = read <$> many1 digit

crate :: Parser Crate
crate = between (char '[') (char ']') anyChar

createOrEmpty :: Parser (Maybe Crate)
createOrEmpty = try (Nothing <$ string "   ") <|> (Just <$> crate)

crateLine :: Parser [Maybe Crate]
crateLine = sepBy createOrEmpty (char ' ') <* newline

crateStacks :: Parser [Stack]
crateStacks = map catMaybes . transpose <$> many1 crateLine

stackNum :: Parser ()
stackNum = void $ many (digit <|> char ' ') <* newline

moveLine :: Parser Move
moveLine = Move <$> parseNumCrates <*> parseFrom <*> parseTo
  where
    parseNumCrates = string "move " *> integer
    parseFrom = string " from " *> integer
    parseTo = string " to " *> integer

parseInput :: Parser ([Stack], [Move])
parseInput = (,) <$> (crateStacks <* stackNum <* newline) <*> sepEndBy moveLine newline <* eof

-- Solver

runMove :: (Stack -> Stack) -> [Stack] -> Move -> [Stack]
runMove f stacks (Move numCrates from to) =
  let fromStack = stacks !! (from - 1)
      toStack = stacks !! (to - 1)
      createsToMove = take numCrates fromStack
      fromStackAfter = drop numCrates fromStack
      toStackAfter = f createsToMove ++ toStack
      stackAfter = take (from - 1) stacks ++ fromStackAfter : drop from stacks
   in take (to - 1) stackAfter ++ toStackAfter : drop to stackAfter

solve :: (Stack -> Stack) -> ([Stack], [Move]) -> String
solve f (stacks, moves) = map head $ foldl (runMove f) stacks moves

part1 :: ([Stack], [Move]) -> IO ()
part1 = print . solve reverse

part2 :: ([Stack], [Move]) -> IO ()
part2 = print . solve id
