module Day21() where
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type MonkeyId = String

data MonkeyYelling = Number Int | Var | Operation Op MonkeyId MonkeyId deriving (Show)

data Op = Add | Sub | Mult | Div deriving (Show)

data Result = Result Int | Equation (Int -> Int)

data OpSide = LeftOp | RightOp

type Monkeys = M.Map MonkeyId MonkeyYelling

-- Helpers

computeOperation :: Op -> Int -> Int -> Int
computeOperation Add = (+)
computeOperation Sub = (-)
computeOperation Div = div
computeOperation Mult = (*)

computeReversedOperation :: OpSide -> Op -> Int -> Int -> Int
computeReversedOperation _ Add = flip (-)
computeReversedOperation LeftOp Sub = (-)
computeReversedOperation RightOp Sub = (+)
computeReversedOperation _ Mult = flip div
computeReversedOperation LeftOp Div = div
computeReversedOperation RightOp Div = (*)

computeYelling :: Monkeys -> MonkeyYelling -> Result
computeYelling _ (Number n) = Result n
computeYelling _ Var = Equation id
computeYelling monkeys (Operation op m1 m2) = operationOrEquation op r1 r2
  where
    y1 = fromJust $ M.lookup m1 monkeys
    y2 = fromJust $ M.lookup m2 monkeys
    r1 = computeYelling monkeys y1
    r2 = computeYelling monkeys y2

operationOrEquation :: Op -> Result -> Result -> Result
operationOrEquation _ (Equation _) (Equation _) = error "Cannot combine two equations"
operationOrEquation op (Result r1) (Result r2) = Result $ computeOperation op r1 r2
operationOrEquation op (Result r) (Equation eq) = Equation (eq . computeReversedOperation LeftOp op r)
operationOrEquation op (Equation eq) (Result r) = Equation (eq . computeReversedOperation RightOp op r)

computeRoot :: Monkeys -> Int
computeRoot monkeys =
  let m = fromJust $ M.lookup "root" monkeys
   in case computeYelling monkeys m of
        Result r -> r
        _ -> error "Invalid yelling result"

computeHumanInput :: Monkeys -> Int
computeHumanInput monkeys =
  let monkeys' = M.insert "humn" Var monkeys
   in case M.lookup "root" monkeys' of
        Just (Operation _ left right) ->
          let ll = fromJust $ M.lookup left monkeys'
              lr = fromJust $ M.lookup right monkeys'
           in computeEquation (computeYelling monkeys' ll) (computeYelling monkeys' lr)
        y -> error ("Invalid monkey yelling " ++ show y)

computeEquation :: Result -> Result -> Int
computeEquation (Result n) (Equation eq) = eq n
computeEquation (Equation eq) (Result n) = eq n
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

--main :: IO ()
--main = parseAndSolve "inputs/day21" part1 part2
