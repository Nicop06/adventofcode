module Day23
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Map.Strict as M (Map, empty, findWithDefault, insert)
import Text.Parsec
import Text.Parsec.String

data Value
  = Value Int
  | Register Char

data Instruction
  = Set Char Value
  | Add Char Value
  | Sub Char Value
  | Mul Char Value
  | Jnz Value Value

type Program = [Instruction]

data ProgramState = ProgramState
  { instructionRegister :: Int,
    registerState :: Map Char Int
  }
  deriving (Show)

initState :: ProgramState
initState =
  ProgramState
    { instructionRegister = 0,
      registerState = M.empty
    }

getValue :: ProgramState -> Value -> Int
getValue _ (Value v) = v
getValue state (Register reg) = findWithDefault 0 reg (registerState state)

modifyRegister :: Char -> (Int -> Int) -> ProgramState -> ProgramState
modifyRegister reg fn state =
  let regValue = getValue state (Register reg)
   in state {registerState = insert reg (fn regValue) (registerState state)}

moveBy :: Int -> ProgramState -> ProgramState
moveBy offset state = state {instructionRegister = instructionRegister state + offset}

execute :: Instruction -> ProgramState -> ProgramState
execute (Set reg v) state = modifyRegister reg (const $ getValue state v) state
execute (Add reg v) state = modifyRegister reg (getValue state v +) state
execute (Sub reg v) state = modifyRegister reg (getValue state v `subtract`) state
execute (Mul reg v) state = modifyRegister reg (getValue state v *) state
execute (Jnz v offset) state
  | getValue state v /= 0 = moveBy (getValue state offset - 1) state
  | otherwise = state

currentInstruction :: Program -> ProgramState -> Maybe Instruction
currentInstruction instructions state
  | insReg < 0 || insReg >= length instructions = Nothing
  | otherwise = Just (instructions !! insReg)
  where
    insReg = instructionRegister state

runProgram :: [Instruction] -> (ProgramState, [Instruction])
runProgram instructions = go initState
  where
    go :: ProgramState -> (ProgramState, [Instruction])
    go state = case currentInstruction instructions state of
      Nothing -> (state, [])
      Just i -> let (s, is) = go (moveBy 1 $ execute i state) in (s, i : is)

isPrime :: Int -> Bool
isPrime k = length [x | x <- [2 .. k], k `mod` x == 0] == 1

parseValue :: Parser Value
parseValue =
  (Value . read <$> many1 (digit <|> char '-'))
    <|> (Register <$> letter)

parseInstruction :: Parser Instruction
parseInstruction =
  try ((Set <$ string "set ") <*> (letter <* char ' ') <*> parseValue)
    <|> ((Add <$ string "add ") <*> (letter <* char ' ') <*> parseValue)
    <|> try ((Mul <$ string "mul ") <*> (letter <* char ' ') <*> parseValue)
    <|> ((Sub <$ string "sub ") <*> (letter <* char ' ') <*> parseValue)
    <|> ((Jnz <$ string "jnz ") <*> (parseValue <* char ' ') <*> parseValue)

parseComment :: Parser String
parseComment = many (char ' ') *> char '#' *> many (noneOf "\n")

parseInput :: Parser [Instruction]
parseInput =
  (parseInstruction <* optional parseComment)
    `sepEndBy1` newline
    <* eof

part1 :: [Instruction] -> IO ()
part1 = print . length . filter isMul . snd . runProgram
  where
    isMul :: Instruction -> Bool
    isMul (Mul _ _) = True
    isMul _ = False

part2 :: [Instruction] -> IO ()
part2 instructions = print . length $ filter (not . isPrime) [b, b + 17 .. b + 17000]
  where
    state = fst $ runProgram instructions
    b = 100000 + 100 * getValue state (Register 'b')
