import qualified Data.Map.Strict as M
import Data.Maybe (maybe)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type MonkeyId = String

data MonkeyYelling = Number Int | Var | Operation Op MonkeyId MonkeyId deriving Show

data Op = Add | Sub | Mult | Div deriving Show

data Result = Result Int | Equation (Int -> Int)

data OpSide = LeftOp | RightOp

type Monkeys = M.Map MonkeyId MonkeyYelling

-- Helpers

computeOperation :: Op -> Int -> Int -> Int
computeOperation Add = (+)
computeOperation Sub = (-)
computeOperation Div = (div)
computeOperation Mult = (*)

computeReversedOperation :: OpSide -> Op -> Int -> Int -> Int
computeReversedOperation _ Add = flip (-)
computeReversedOperation LeftOp Sub = (-)
computeReversedOperation RightOp Sub = (+)
computeReversedOperation _ Mult = flip div
computeReversedOperation LeftOp Div = (div)
computeReversedOperation RightOp Div = (*)

computeYelling :: Monkeys -> MonkeyYelling -> Result
computeYelling _ (Number n) = Result n
computeYelling _ Var = Equation id
computeYelling monkeys (Operation op m1 m2) = operationOrEquation op r1 r2
    where Just y1 = M.lookup m1 monkeys
          Just y2 = M.lookup m2 monkeys
          r1 = computeYelling monkeys y1
          r2 = computeYelling monkeys y2

operationOrEquation :: Op -> Result -> Result -> Result
operationOrEquation op (Equation e1) (Equation e2) = error "Cannot combine two equations"
operationOrEquation op (Result r1) (Result r2) = Result $ computeOperation op r1 r2
operationOrEquation op (Result r) (Equation eq) = Equation (eq . (computeReversedOperation LeftOp op r))
operationOrEquation op (Equation eq) (Result r) = Equation (eq . (computeReversedOperation RightOp op r))

computeRoot :: Monkeys -> Int
computeRoot monkeys =
    let Just m = M.lookup "root" monkeys
        Result r = computeYelling monkeys m
        in r

computeHumanInput :: Monkeys -> Int
computeHumanInput monkeys =
    let monkeys' = M.insert "humn" Var monkeys
        Just (Operation op left right) = M.lookup "root" monkeys'
        Just ll = M.lookup left monkeys'
        Just lr = M.lookup right monkeys'
    in computeEquation (computeYelling monkeys' ll) (computeYelling monkeys' lr)

computeEquation :: Result -> Result -> Int
computeEquation (Result n) (Equation eq) = eq $ n
computeEquation (Equation eq) (Result n) = eq $ n
computeEquation _ _ = error "Need one equation and one result"

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

part2 :: Parser Int
part2 = computeHumanInput <$> parseInput

main :: IO ()
main = parseAndSolve "inputs/day21" part1 part2
