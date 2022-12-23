import qualified Data.Map.Strict as M
import Data.Maybe (maybe)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type MonkeyId = String

data MonkeyYelling = Number Int | Operation Op MonkeyId MonkeyId deriving Show

data Op = Add | Sub | Mult | Div deriving Show

type Monkeys = M.Map MonkeyId MonkeyYelling

-- Helpers

computeOperation :: Op -> Int -> Int -> Int
computeOperation Add = (+)
computeOperation Sub = (-)
computeOperation Div = (div)
computeOperation Mult = (*)

computeYelling :: Monkeys -> MonkeyYelling -> Int
computeYelling _ (Number n) = n
computeYelling monkeys (Operation op m1 m2) = computeOperation op r1 r2
    where Just y1 = M.lookup m1 monkeys
          Just y2 = M.lookup m2 monkeys
          r1 = computeYelling monkeys y1
          r2 = computeYelling monkeys y2

computeRoot :: Monkeys -> Int
computeRoot monkeys = let Just m = M.lookup "root" monkeys in computeYelling monkeys m

-- Parser

parseAdd :: Parser Op
parseAdd = Add <$ char '+'

parseSub :: Parser Op
parseSub = Sub <$ char '-'

parseDiv :: Parser Op
parseDiv = Div <$ char '/'

parseMult :: Parser Op
parseMult = Mult <$ char '*'

parseOp :: Parser Op
parseOp = char ' ' *> (parseAdd <|> parseSub <|> parseDiv <|> parseMult) <* char ' '

number :: Parser Int
number = read <$> many1 digit

monkeyId :: Parser MonkeyId
monkeyId = many1 lower

monkeyYelling :: Parser MonkeyYelling
monkeyYelling = (Number <$> number) <|> (flip Operation <$> monkeyId <*> parseOp <*> monkeyId)

parseMonkey :: Parser (MonkeyId, MonkeyYelling)
parseMonkey = (,) <$> (monkeyId <* string ": ") <*> monkeyYelling

parseInput :: Parser Monkeys
parseInput = M.fromList <$> parseMonkey `sepEndBy1` newline <* eof

part1 :: Parser Int
part1 = computeRoot <$> parseInput

part2 = return 0

main :: IO ()
main = parseAndSolve "inputs/day21" part1 part2
