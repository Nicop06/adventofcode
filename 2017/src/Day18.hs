module Day18
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Map as M (Map, empty, findWithDefault, insert, singleton)
import Data.Maybe (catMaybes, fromMaybe)
import Text.Parsec
import Text.Parsec.String

data Value
  = Value Int
  | Register Char
  deriving (Show, Eq)

data Instruction
  = Send Value
  | Receive Char
  | Set Char Value
  | Add Char Value
  | Mul Char Value
  | Mod Char Value
  | JumpGTZero Value Value
  deriving (Show, Eq)

type Program = [Instruction]

data ProgramState = ProgramState
  { instructionRegister :: Int,
    outputValue :: Maybe Int,
    registerState :: Map Char Int,
    inputQueue :: [Int]
  }
  deriving (Show, Eq)

initState :: ProgramState
initState =
  ProgramState
    { instructionRegister = 0,
      outputValue = Nothing,
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
execute (Send v) state = state {outputValue = Just (getValue state v)}
execute (Receive reg) state = case inputQueue state of
  [] -> moveBy (-1) state
  (v : vs) -> (modifyRegister reg (const v) state) {inputQueue = vs}
execute (JumpGTZero v offset) state
  | getValue state v > 0 = moveBy (getValue state offset - 1) state
  | otherwise = state

currentInstruction :: Program -> ProgramState -> Maybe Instruction
currentInstruction instructions state
  | insReg < 0 || insReg >= length instructions = Nothing
  | otherwise = Just (instructions !! insReg)
  where
    insReg = instructionRegister state

executeUntilFirstRecover :: Program -> Int
executeUntilFirstRecover instructions = fromMaybe 0 (go initState)
  where
    go :: ProgramState -> Maybe Int
    go state = case currentInstruction instructions state of
      Nothing -> outputValue state
      Just (Receive reg) ->
        if getValue state (Register reg) == 0
          then go (moveBy 1 state)
          else outputValue state
      Just ins -> go (moveBy 1 $ execute ins state)

numOutputs :: ProgramState -> Int
numOutputs state = case outputValue state of
  Nothing -> 0
  (Just _) -> 1

executeTwoPrograms :: Program -> Int
executeTwoPrograms instructions = go (initProgram 0, initProgram 1)
  where
    initProgram :: Int -> ProgramState
    initProgram p = initState {registerState = M.singleton 'p' p}

    nextState :: ProgramState -> Instruction -> ProgramState
    nextState state instruction = moveBy 1 (execute instruction state)

    moveOutput :: Maybe Int -> ProgramState -> ProgramState
    moveOutput v state = state {outputValue = Nothing, inputQueue = inputQueue state ++ catMaybes [v]}

    go :: (ProgramState, ProgramState) -> Int
    go (stateP0, stateP1) = case (currentInstruction instructions stateP0, currentInstruction instructions stateP1) of
      (_, Nothing) -> 0
      (Nothing, Just ins) ->
        let newState = nextState stateP1 ins
         in numOutputs newState + go (stateP0, moveOutput Nothing newState)
      (Just (Receive reg0), Just (Receive reg1)) ->
        if null (inputQueue stateP0) && null (inputQueue stateP1)
          then 0
          else continue stateP0 stateP1 (Receive reg0) (Receive reg1)
      (Just ins0, Just ins1) -> continue stateP0 stateP1 ins0 ins1

    continue :: ProgramState -> ProgramState -> Instruction -> Instruction -> Int
    continue stateP0 stateP1 ins0 ins1 =
      let newStateP0 = nextState stateP0 ins0
          newStateP1 = nextState stateP1 ins1
       in numOutputs newStateP1
            + go
              ( moveOutput (outputValue newStateP1) newStateP0,
                moveOutput (outputValue newStateP0) newStateP1
              )

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
    <|> ((Send <$ string "snd ") <*> parseValue)
    <|> ((Receive <$ string "rcv ") <*> letter)

parseInput :: Parser [Instruction]
parseInput = parseInstruction `sepEndBy1` newline <* eof

part1 :: [Instruction] -> IO ()
part1 = print . executeUntilFirstRecover

part2 :: [Instruction] -> IO ()
part2 = print . executeTwoPrograms
