module Day23
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

-- Data

data Register = A | B deriving (Show)

type Offset = Int

data Instruction
  = Half Register
  | Triple Register
  | Increment Register
  | Jump Offset
  | JumpIfEven Register Offset
  | JumpIfOne Register Offset
  deriving (Show)

data Code = Code {prevInstructions :: [Instruction], nextInstructions :: [Instruction]} deriving (Show)

data Memory = Memory {aReg :: Int, bReg :: Int} deriving (Show)

data CodeState = CodeState {memory :: Memory, code :: Code} deriving (Show)

-- Helpers

executeCode :: CodeState -> [CodeState]
executeCode state = case code state of
  (Code _ []) -> []
  (Code _ (i : _)) -> state : executeCode (executeInstruction i state)

executeInstruction :: Instruction -> CodeState -> CodeState
executeInstruction (Half reg) = changeRegister (`div` 2) reg . jump 1
executeInstruction (Triple reg) = changeRegister (* 3) reg . jump 1
executeInstruction (Increment reg) = changeRegister (+ 1) reg . jump 1
executeInstruction (Jump offset) = jump offset
executeInstruction (JumpIfEven reg offset) = conditionalJump even reg offset
executeInstruction (JumpIfOne reg offset) = conditionalJump (== 1) reg offset

jump :: Offset -> CodeState -> CodeState
jump offset state
  | offset < 0 && not (null prev) = jump (offset + 1) $ state {code = Code (tail prev) (head prev : next)}
  | offset > 0 && not (null next) = jump (offset - 1) $ state {code = Code (head next : prev) (tail next)}
  | otherwise = state
  where
    (Code prev next) = code state

conditionalJump :: (Int -> Bool) -> Register -> Offset -> CodeState -> CodeState
conditionalJump f reg offset state = if f $ regValue reg then jump offset state else jump 1 state
  where
    mem = memory state
    regValue A = aReg mem
    regValue B = aReg mem

changeRegister :: (Int -> Int) -> Register -> CodeState -> CodeState
changeRegister f reg state =
  let memState = memory state
   in case reg of
        A -> state {memory = memState {aReg = f $ aReg memState}}
        B -> state {memory = memState {bReg = f $ bReg memState}}

-- Parser

parseRegister :: Parser Register
parseRegister = (A <$ char 'a') <|> (B <$ char 'b')

parseOffset :: Parser Offset
parseOffset = read <$> (optional (char '+') *> many1 (digit <|> char '-'))

parseHalf :: Parser Instruction
parseHalf = Half <$> try (string "hlf " *> parseRegister)

parseTriple :: Parser Instruction
parseTriple = Triple <$> try (string "tpl " *> parseRegister)

parseInc :: Parser Instruction
parseInc = Increment <$> try (string "inc " *> parseRegister)

parseJump :: Parser Instruction
parseJump = Jump <$> try (string "jmp " *> parseOffset)

parseJumpIfEven :: Parser Instruction
parseJumpIfEven = JumpIfEven <$> try (string "jie " *> parseRegister) <*> (string ", " *> parseOffset)

parseJumpIfOne :: Parser Instruction
parseJumpIfOne = JumpIfOne <$> try (string "jio " *> parseRegister) <*> (string ", " *> parseOffset)

parseInstruction :: Parser Instruction
parseInstruction = parseHalf <|> parseTriple <|> parseInc <|> parseJump <|> parseJumpIfEven <|> parseJumpIfOne

parseInput :: Parser Code
parseInput = Code [] <$> parseInstruction `sepEndBy1` newline <* eof

part1 :: Code -> IO ()
part1 = print . bReg . memory . last . executeCode . CodeState (Memory 0 0)

part2 :: Code -> IO ()
part2 = print . bReg . memory . last . executeCode . CodeState (Memory 1 0)
