module Day18 (
  parseInput,
  part1,
  part2,
)
where

import Data.Map as M (Map, empty, findWithDefault, insert)
import Text.Parsec
import Text.Parsec.String

data Value
  = Value Int
  | Register Char
  deriving (Show, Eq)

data Instruction
  = Sound Value
  | Recover Char
  | Set Char Value
  | Add Char Value
  | Mul Char Value
  | Mod Char Value
  | JumpGTZero Value Value
  deriving (Show, Eq)

type Program = [Instruction]

data ProgramState = ProgramState
  { instructionRegister :: Int
  , lastSound :: Int
  , registerState :: Map Char Int
  }
  deriving (Show, Eq)

initState :: ProgramState
initState =
  ProgramState
    { instructionRegister = 0
    , lastSound = 0
    , registerState = M.empty
    }

getValue :: ProgramState -> Value -> Int
getValue _ (Value v) = v
getValue state (Register reg) = findWithDefault 0 reg (registerState state)

modifyRegister :: Char -> (Int -> Int) -> ProgramState -> ProgramState
modifyRegister reg fn state =
  let regValue = getValue state (Register reg)
   in state{registerState = insert reg (fn regValue) (registerState state)}

moveBy :: Int -> ProgramState -> ProgramState
moveBy offset state = state{instructionRegister = instructionRegister state + offset}

execute :: Instruction -> ProgramState -> ProgramState
execute (Set reg v) state = modifyRegister reg (const $ getValue state v) state
execute (Add reg v) state = modifyRegister reg (getValue state v +) state
execute (Mul reg v) state = modifyRegister reg (getValue state v *) state
execute (Mod reg v) state = modifyRegister reg (`mod` getValue state v) state
execute (Sound v) state = state{lastSound = getValue state v}
execute (Recover reg) state
  | getValue state (Register reg) == 0 = state
  | otherwise = modifyRegister reg (const $ lastSound state) state
execute (JumpGTZero v offset) state
  | getValue state v > 0 = moveBy (getValue state offset - 1) state
  | otherwise = state

executeUntilFirstRecover :: Program -> ProgramState
executeUntilFirstRecover instructions = go initState
 where
  currentInstruction :: ProgramState -> Instruction
  currentInstruction state = instructions !! instructionRegister state

  executeAndMove :: ProgramState -> ProgramState
  executeAndMove state = moveBy 1 (execute (currentInstruction state) state)

  go :: ProgramState -> ProgramState
  go state
    | instructionRegister state < 0 || instructionRegister state >= length instructions = state
    | otherwise = case currentInstruction state of
        Recover reg ->
          let newState = executeAndMove state
           in if getValue state (Register reg) == 0
                then go newState
                else newState
        _ -> go (executeAndMove state)

parseValue :: Parser Value
parseValue =
  (Value . read <$> many1 (digit <|> char '-'))
    <|> (Register <$> letter)

parseInstruction :: Parser Instruction
parseInstruction =
  try ((Set <$ string "set ") <*> (letter <* char ' ') <*> parseValue)
    <|> ((Add <$ string "add ") <*> (letter <* char ' ') <*> parseValue)
    <|> try ((Mul <$ string "mul ") <*> (letter <* char ' ') <*> parseValue)
    <|> ((Mod <$ string "mod ") <*> (letter <* char ' ') <*> parseValue)
    <|> ((JumpGTZero <$ string "jgz ") <*> (parseValue <* char ' ') <*> parseValue)
    <|> ((Sound <$ string "snd ") <*> parseValue)
    <|> ((Recover <$ string "rcv ") <*> letter)

parseInput :: Parser [Instruction]
parseInput = parseInstruction `sepEndBy1` newline <* eof

part1 :: [Instruction] -> IO ()
part1 = print . lastSound . executeUntilFirstRecover

part2 :: [Instruction] -> IO ()
part2 = print . length
