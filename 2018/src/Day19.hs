module Day19
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Bits
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String

data RegState = RegState
  { reg0 :: Int,
    reg1 :: Int,
    reg2 :: Int,
    reg3 :: Int,
    reg4 :: Int,
    reg5 :: Int
  }
  deriving (Show, Eq)

data OperationType
  = AddR
  | AddI
  | MulR
  | MulI
  | BanR
  | BanI
  | BorR
  | BorI
  | SetR
  | SetI
  | GtIR
  | GtRI
  | GtRR
  | EqIR
  | EqRI
  | EqRR
  deriving (Show, Eq, Ord)

-- A -> B -> C -> RegState -> Maybe RegState
type Operation = Int -> Int -> Int -> RegState -> Maybe RegState

type GetVal = Int -> RegState -> Maybe Int

type Cond = Int -> Int -> Bool

data Instruction = Instruction
  { getOperation :: OperationType,
    getValA :: Int,
    getValB :: Int,
    getValC :: Int
  }
  deriving (Show)

data Program = Program
  { ipReg :: Int,
    instructions :: V.Vector Instruction
  }
  deriving (Show)

get :: GetVal
get 0 = Just . reg0
get 1 = Just . reg1
get 2 = Just . reg2
get 3 = Just . reg3
get 4 = Just . reg4
get 5 = Just . reg5
get _ = const Nothing

imm :: GetVal
imm = const . Just

set :: Int -> RegState -> Int -> Maybe RegState
set i state v = case i of
  0 -> Just $ state {reg0 = v}
  1 -> Just $ state {reg1 = v}
  2 -> Just $ state {reg2 = v}
  3 -> Just $ state {reg3 = v}
  4 -> Just $ state {reg4 = v}
  5 -> Just $ state {reg5 = v}
  _ -> Nothing

opx :: GetVal -> GetVal -> (Int -> Int -> Int) -> Operation
opx geta getb op a b c state = set c state =<< (op <$> geta a state <*> getb b state)

opi :: (Int -> Int -> Int) -> Operation
opi = opx get imm

opr :: (Int -> Int -> Int) -> Operation
opr = opx get get

cond :: GetVal -> GetVal -> Cond -> Operation
cond geta getb op a b c state = case op <$> geta a state <*> getb b state of
  Nothing -> Nothing
  Just True -> set c state 1
  Just False -> set c state 0

------------------
-- Operations --
------------------

operation :: OperationType -> Operation
operation AddR = opr (+)
operation AddI = opi (+)
operation MulR = opr (*)
operation MulI = opi (*)
operation BanR = opr (.&.)
operation BanI = opi (.&.)
operation BorR = opr (.|.)
operation BorI = opi (.|.)
operation SetR = opi const
operation SetI = opx imm imm const
operation GtIR = cond imm get (>)
operation GtRI = cond get imm (>)
operation GtRR = cond get get (>)
operation EqIR = cond imm get (==)
operation EqRI = cond get imm (==)
operation EqRR = cond get get (==)

runInstruction :: Instruction -> RegState -> Maybe RegState
runInstruction (Instruction op a b c) = operation op a b c

runProgram :: (RegState -> Bool) -> Program -> RegState -> RegState
runProgram stopCond (Program ip ins) = go
  where
    getInstruction :: RegState -> Maybe Instruction
    getInstruction state = get ip state >>= (V.!?) ins

    nextInstruction :: Instruction
    nextInstruction = Instruction AddI ip 1 ip

    go :: RegState -> RegState
    go state
      | stopCond state = state
      | otherwise =
          maybe
            state
            go
            ( getInstruction state
                >>= flip runInstruction state
                >>= runInstruction nextInstruction
            )

sumPrimeFactors :: Int -> Int
sumPrimeFactors n = sum [p | p <- [1 .. n], n `mod` p == 0]

------------
-- Parser --
------------

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseOperation :: Parser OperationType
parseOperation =
  try (AddR <$ string "addr")
    <|> try (AddI <$ string "addi")
    <|> try (MulR <$ string "mulr")
    <|> try (MulI <$ string "muli")
    <|> try (BanR <$ string "banr")
    <|> try (BanI <$ string "bani")
    <|> try (BorR <$ string "borr")
    <|> try (BorI <$ string "bori")
    <|> try (SetR <$ string "setr")
    <|> try (SetI <$ string "seti")
    <|> try (GtIR <$ string "gtir")
    <|> try (GtRI <$ string "gtri")
    <|> try (GtRR <$ string "gtrr")
    <|> try (EqIR <$ string "eqir")
    <|> try (EqRI <$ string "eqri")
    <|> try (EqRR <$ string "eqrr")

parseInstruction :: Parser Instruction
parseInstruction =
  Instruction
    <$> (parseOperation <* char ' ')
    <*> (parseInt <* char ' ')
    <*> (parseInt <* char ' ')
    <*> parseInt

parseInput :: Parser Program
parseInput =
  Program
    <$> between (string "#ip ") newline parseInt
    <*> (V.fromList <$> parseInstruction `sepEndBy1` newline)
    <* eof

part1 :: Program -> IO ()
part1 = print . reg0 . flip (runProgram (const False)) initState
  where
    initState :: RegState
    initState = RegState 0 0 0 0 0 0

part2 :: Program -> IO ()
part2 = print . sumPrimeFactors . reg5 . flip (runProgram stopCond) initState
  where
    initState :: RegState
    initState = RegState 1 0 0 0 0 0

    stopCond :: RegState -> Bool
    stopCond state = reg4 state == 1
