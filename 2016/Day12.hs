module Day12
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

data Instruction = Copy Value Register | Inc Register | Dec Register | JumpNoZero Value Int deriving (Show, Eq)

type InstructionState = ([Instruction], [Instruction])

data GlobalState = GlobalState {regState :: RegState, insState :: InstructionState} deriving (Show, Eq)

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
goUp (u, i : d) = (i : u, d)
goUp (u, []) = (u, [])

goDown :: InstructionState -> InstructionState
goDown (i : u, d) = (u, i : d)
goDown ([], d) = ([], d)

moveBy :: Int -> InstructionState -> InstructionState
moveBy i
  | i > 0 = moveBy (i - 1) . goDown
  | i < 0 = moveBy (i + 1) . goUp
  | otherwise = id

execute :: Instruction -> GlobalState -> GlobalState
execute (Copy from to) s@(GlobalState rs _) = s {regState = copy from to rs}
execute (Inc reg) s@(GlobalState rs _) = s {regState = increment reg rs}
execute (Dec reg) s@(GlobalState rs _) = s {regState = decrement reg rs}
execute (JumpNoZero val n) s@(GlobalState rs ins) =
  if getValue val rs == 0 then s else s {insState = moveBy (n - 1) ins}

executeNext :: GlobalState -> GlobalState
executeNext s@(GlobalState _ (i : _, _)) = execute i s
executeNext s = s

nextInstruction :: GlobalState -> GlobalState
nextInstruction s@(GlobalState _ i) = s {insState = goDown i}

isEndState :: GlobalState -> Bool
isEndState (GlobalState _ ([], _)) = True
isEndState (GlobalState _ _) = False

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x : rs) = if f x then [x] else x : takeUntil f rs

executeUntilEnd :: RegState -> [Instruction] -> [GlobalState]
executeUntilEnd r i = takeUntil isEndState . iterate (nextInstruction . executeNext) $ GlobalState r (i, [])
 
parseInput :: Parser [Instruction]
parseInput = parseInstruction `sepEndBy1` newline <* eof

parseInstruction :: Parser Instruction
parseInstruction = parseCpy <|> parseInc <|> parseDec <|> parseJnz

parseCpy :: Parser Instruction
parseCpy = Copy <$> (string "cpy " *> parseValue <* char ' ') <*> parseRegister

parseInc :: Parser Instruction
parseInc = Inc <$> (string "inc " *> parseRegister)

parseDec :: Parser Instruction
parseDec = Dec <$> (string "dec " *> parseRegister)

parseJnz :: Parser Instruction
parseJnz = JumpNoZero <$> (string "jnz " *> parseValue <* char ' ') <*> parseNum

parseValue :: Parser Value
parseValue = (Register <$> parseRegister) <|> (Value <$> parseNum)

parseRegister :: Parser Register
parseRegister = read . pure . toUpper <$> oneOf "abcd"

parseNum :: Parser Int
parseNum = read <$> many1 (digit <|> char '-')

executeAndPrintA :: RegState -> [Instruction] -> IO ()
executeAndPrintA r = print . getReg A . regState . last . executeUntilEnd r

part1 :: [Instruction] -> IO ()
part1 = executeAndPrintA (RegState 0 0 0 0)


part2 :: [Instruction] -> IO ()
part2 = executeAndPrintA (RegState 0 0 1 0)
