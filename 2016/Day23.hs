module Day23
  ( parseInput
  , part1
  , part2
  ) where

import Data.Char (toUpper)
import Text.Parsec
import Text.Parsec.String

data Register
  = A
  | B
  | C
  | D
  deriving (Show, Read, Eq)

data RegState =
  RegState
    { regA :: Int
    , regB :: Int
    , regC :: Int
    , regD :: Int
    }
  deriving (Show, Eq)

data Value
  = Value Int
  | Register Register
  deriving (Show, Eq)

data Instruction
  = NoOp
  | Copy Value Register
  | Inc Register
  | Dec Register
  | JumpNoZero Value Value
  | Toggle Value
  deriving (Show, Eq)

type InstructionState = ([Instruction], [Instruction])

data GlobalState =
  GlobalState
    { regState :: RegState
    , insState :: InstructionState
    }
  deriving (Show, Eq)

setReg :: Register -> Int -> RegState -> RegState
setReg A x rs = rs {regA = x}
setReg B x rs = rs {regB = x}
setReg C x rs = rs {regC = x}
setReg D x rs = rs {regD = x}

getReg :: Register -> RegState -> Int
getReg A = regA
getReg B = regB
getReg C = regC
getReg D = regD

getValue :: Value -> RegState -> Int
getValue (Value x) = const x
getValue (Register reg) = getReg reg

increment :: Register -> RegState -> RegState
increment reg rs = setReg reg (getReg reg rs + 1) rs

decrement :: Register -> RegState -> RegState
decrement reg rs = setReg reg (getReg reg rs - 1) rs

copy :: Value -> Register -> RegState -> RegState
copy from to rs = setReg to (getValue from rs) rs

goUp :: InstructionState -> InstructionState
goUp (u, i:d) = (i : u, d)
goUp (u, []) = (u, [])

goDown :: InstructionState -> InstructionState
goDown (i:u, d) = (u, i : d)
goDown ([], d) = ([], d)

moveBy :: Int -> InstructionState -> InstructionState
moveBy by state@(_, _)
  | by < 0 = moveBy (by + 1) . goUp $ state
  | by > 0 = moveBy (by - 1) . goDown $ state
  | otherwise = state

toggle :: Instruction -> Instruction
toggle NoOp = NoOp
toggle (Copy from to) = JumpNoZero from (Register to)
toggle (JumpNoZero val (Register reg)) = Copy val reg
toggle (JumpNoZero _ _) = NoOp
toggle (Inc reg) = Dec reg
toggle (Dec reg) = Inc reg
toggle (Toggle (Register reg)) = Inc reg
toggle (Toggle (Value _)) = NoOp

toggleAt :: Int -> InstructionState -> InstructionState
toggleAt _ ([], []) = ([], [])
toggleAt at state@([], _:_)
  | at < 0 = goDown . toggleAt (at + 1) . goUp $ state
  | otherwise = state
toggleAt at state@(i:u, [])
  | at > 0 = goUp . toggleAt (at - 1) . goDown $ state
  | at == 0 = (toggle i : u, [])
  | otherwise = state
toggleAt at state@(i:u, j:d)
  | at < 0 = goDown . toggleAt (at + 1) . goUp $ state
  | at > 0 = goUp . toggleAt (at - 1) . goDown $ state
  | otherwise = (toggle i : u, j : d)

execute :: Instruction -> GlobalState -> GlobalState
execute NoOp s = nextInstruction s
execute (Copy from to) s@(GlobalState rs _) =
  nextInstruction $ s {regState = copy from to rs}
execute (Inc reg) s@(GlobalState rs _) =
  nextInstruction $ s {regState = increment reg rs}
execute (Dec reg) s@(GlobalState rs _) =
  nextInstruction $ s {regState = decrement reg rs}
execute (JumpNoZero val n) s@(GlobalState rs ins) =
  if getValue val rs == 0
    then nextInstruction s
    else s {insState = moveBy (getValue n rs) ins}
execute (Toggle val) s@(GlobalState rs ins) =
  nextInstruction $ s {insState = toggleAt (getValue val rs) ins}

executeNext :: GlobalState -> GlobalState
executeNext s@(GlobalState _ (i:_, _)) = execute i s
executeNext s = s

nextInstruction :: GlobalState -> GlobalState
nextInstruction s@(GlobalState _ i) = s {insState = goDown i}

executeUntilEnd :: GlobalState -> GlobalState
executeUntilEnd state@(GlobalState _ ([], _)) = state
executeUntilEnd state@(GlobalState rs _)
  | (getReg A rs > 0) = executeUntilEnd $! executeNext state -- Force strictness
  | otherwise = executeUntilEnd $! executeNext state

makeInitState :: RegState -> [Instruction] -> GlobalState
makeInitState r i = GlobalState r (i, [])

parseInput :: Parser [Instruction]
parseInput = parseInstruction `sepEndBy1` newline <* eof

parseInstruction :: Parser Instruction
parseInstruction = parseCpy <|> parseInc <|> parseDec <|> parseJnz <|> parseTgl

parseTgl :: Parser Instruction
parseTgl = Toggle <$> (string "tgl " *> parseValue)

parseCpy :: Parser Instruction
parseCpy = Copy <$> (string "cpy " *> parseValue <* char ' ') <*> parseRegister

parseInc :: Parser Instruction
parseInc = Inc <$> (string "inc " *> parseRegister)

parseDec :: Parser Instruction
parseDec = Dec <$> (string "dec " *> parseRegister)

parseJnz :: Parser Instruction
parseJnz =
  JumpNoZero <$> (string "jnz " *> parseValue <* char ' ') <*> parseValue

parseValue :: Parser Value
parseValue = (Register <$> parseRegister) <|> (Value <$> parseNum)

parseRegister :: Parser Register
parseRegister = read . pure . toUpper <$> oneOf "abcd"

parseNum :: Parser Int
parseNum = read <$> many1 (digit <|> char '-')

executeAndPrintA :: RegState -> [Instruction] -> IO ()
executeAndPrintA r =
  print . getReg A . regState . executeUntilEnd . makeInitState r

part1 :: [Instruction] -> IO ()
part1 = executeAndPrintA (RegState 7 0 0 0)

part2 :: [Instruction] -> IO ()
part2 = executeAndPrintA (RegState 12 0 0 0)
