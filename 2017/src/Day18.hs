module Day18
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Map as M (Map, empty, findWithDefault, insert, singleton)
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String

data Network = Receive | Send Int

data Value
  = Value Int
  | Register Char

data Instruction
  = Snd Value
  | Rcv Char
  | Set Char Value
  | Add Char Value
  | Mul Char Value
  | Mod Char Value
  | Jgz Value Value

type Program = [Instruction]

data ProgramState = ProgramState
  { instructionRegister :: Int,
    outputValue :: Maybe Int,
    registerState :: Map Char Int,
    inputQueue :: [Network]
  }

initState :: ProgramState
initState =
  ProgramState
    { instructionRegister = 0,
      outputValue = Just 0,
      registerState = M.empty,
      inputQueue = []
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
execute (Mul reg v) state = modifyRegister reg (getValue state v *) state
execute (Mod reg v) state = modifyRegister reg (`mod` getValue state v) state
execute (Snd v) state = state {outputValue = Just $ getValue state v}
execute (Rcv _) state = state
execute (Jgz v offset) state
  | getValue state v > 0 = moveBy (getValue state offset - 1) state
  | otherwise = state

currentInstruction :: Program -> ProgramState -> Maybe Instruction
currentInstruction instructions state
  | insReg < 0 || insReg >= length instructions = Nothing
  | otherwise = Just (instructions !! insReg)
  where
    insReg = instructionRegister state

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
    <|> ((Jgz <$ string "jgz ") <*> (parseValue <* char ' ') <*> parseValue)
    <|> ((Snd <$ string "snd ") <*> parseValue)
    <|> ((Rcv <$ string "rcv ") <*> letter)

parseInput :: Parser [Instruction]
parseInput = parseInstruction `sepEndBy1` newline <* eof

part1 :: [Instruction] -> IO ()
part1 instructions = print $ fromMaybe 0 $ go initState
  where
    go :: ProgramState -> Maybe Int
    go state = case currentInstruction instructions state of
      Nothing -> outputValue state
      Just (Rcv reg) ->
        if getValue state (Register reg) == 0
          then go (moveBy 1 state)
          else outputValue state
      Just ins -> go (moveBy 1 $ execute ins state)

part2 :: [Instruction] -> IO ()
part2 instructions =
  let p0 = runProgram (program 0 p1)
      p1 = runProgram (program 1 p0)
   in print $ length $ filter isSend p1
  where
    isSend :: Network -> Bool
    isSend (Send _) = True
    isSend _ = False

    program :: Int -> [Network] -> ProgramState
    program p q = initState {registerState = M.singleton 'p' p, inputQueue = q}

    runProgram :: ProgramState -> [Network]
    runProgram state = case currentInstruction instructions state of
      Nothing -> []
      Just (Rcv reg) ->
        Receive : case inputQueue state of
          (Send v : rs) -> runProgram (moveBy 1 $ modifyRegister reg (const v) state {inputQueue = rs})
          _ -> []
      Just (Snd v) ->
        Send (getValue state v)
          : runProgram (moveBy 1 state {inputQueue = dropReceive (inputQueue state)})
      Just ins -> runProgram (moveBy 1 $ execute ins state)

    dropReceive :: [Network] -> [Network]
    dropReceive [] = []
    dropReceive (Receive : rs) = rs
    dropReceive (r : rs) = r : dropReceive rs
