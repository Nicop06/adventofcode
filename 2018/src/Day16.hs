{-# LANGUAGE ScopedTypeVariables #-}

module Day16
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.Bits
import qualified Data.Map as M
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

data RegState = RegState
  { reg0 :: Int,
    reg1 :: Int,
    reg2 :: Int,
    reg3 :: Int
  }
  deriving (Show, Eq)

data InstructionType
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
  deriving (Eq, Ord)

-- A -> B -> C -> RegState -> Maybe RegState
type Instruction = Int -> Int -> Int -> RegState -> Maybe RegState

type GetVal = Int -> RegState -> Maybe Int

type Cond = Int -> Int -> Bool

data Operation = Operation
  { getOpCode :: Int,
    getValA :: Int,
    getValB :: Int,
    getValC :: Int
  }
  deriving (Show)

data Sample = Sample
  { getBefore :: RegState,
    getOperation :: Operation,
    getAfter :: RegState
  }
  deriving (Show)

data Puzzle = Puzzle [Sample] [Operation] deriving (Show)

get :: GetVal
get 0 = Just . reg0
get 1 = Just . reg1
get 2 = Just . reg2
get 3 = Just . reg3
get _ = const Nothing

imm :: GetVal
imm = const . Just

set :: Int -> RegState -> Int -> Maybe RegState
set i state v = case i of
  0 -> Just $ state {reg0 = v}
  1 -> Just $ state {reg1 = v}
  2 -> Just $ state {reg2 = v}
  3 -> Just $ state {reg3 = v}
  _ -> Nothing

opx :: GetVal -> GetVal -> (Int -> Int -> Int) -> Instruction
opx geta getb op a b c state = set c state =<< (op <$> geta a state <*> getb b state)

opi :: (Int -> Int -> Int) -> Instruction
opi = opx get imm

opr :: (Int -> Int -> Int) -> Instruction
opr = opx get get

cond :: GetVal -> GetVal -> Cond -> Instruction
cond geta getb op a b c state = case op <$> geta a state <*> getb b state of
  Nothing -> Nothing
  Just True -> set c state 1
  Just False -> set c state 0

------------------
-- Instructions --
------------------

allInstructions :: M.Map InstructionType Instruction
allInstructions =
  M.fromList
    [ (AddR, opr (+)),
      (AddI, opi (+)),
      (MulR, opr (*)),
      (MulI, opi (*)),
      (BanR, opr (.&.)),
      (BanI, opi (.&.)),
      (BorR, opr (.|.)),
      (BorI, opi (.|.)),
      (SetR, opi const),
      (SetI, opx imm imm const),
      (GtIR, cond imm get (>)),
      (GtRI, cond get imm (>)),
      (GtRR, cond get get (>)),
      (EqIR, cond imm get (==)),
      (EqRI, cond get imm (==)),
      (EqRR, cond get get (==))
    ]

getInstruction :: InstructionType -> Instruction
getInstruction = (M.!) allInstructions

matchInstruction :: Sample -> Instruction -> Bool
matchInstruction (Sample before (Operation _ a b c) after) ins = ins a b c before == Just after

findAssignment :: forall a b. Eq b => [(a, [b])] -> [[(a, b)]]
findAssignment [] = []
findAssignment [(a, [b])] = [[(a, b)]]
findAssignment ((a, bs) : rs) = go bs
  where
    go :: [b] -> [[(a, b)]]
    go [] = []
    go (x : xs) =
      map ((a, x) :) (findAssignment (map (second (filter (/= x))) rs)) ++ go xs

resolveInstructions :: [Sample] -> M.Map Int Instruction
resolveInstructions =
  M.fromList . map (second getInstruction) . elemOrError . findAssignment . M.assocs . go instructions
  where
    elemOrError :: [a] -> a
    elemOrError [x] = x
    elemOrError xs = error $ "Found " ++ show (length xs) ++ " elements"

    instructions :: M.Map Int [InstructionType]
    instructions = M.fromList (map (,M.keys allInstructions) [0 .. length allInstructions - 1])

    go :: M.Map Int [InstructionType] -> [Sample] -> M.Map Int [InstructionType]
    go insMap [] = insMap
    go insMap (s : ss) =
      go
        (M.adjust (filter (matchInstruction s . getInstruction)) (getOpCode (getOperation s)) insMap)
        ss

------------
-- Parser --
------------

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseState :: Parser RegState
parseState =
  RegState
    <$> (parseInt <* string ", ")
    <*> (parseInt <* string ", ")
    <*> (parseInt <* string ", ")
    <*> parseInt

parseOperation :: Parser Operation
parseOperation =
  Operation
    <$> (parseInt <* char ' ')
    <*> (parseInt <* char ' ')
    <*> (parseInt <* char ' ')
    <*> parseInt

parseSample :: Parser Sample
parseSample =
  Sample
    <$> between (string "Before: [") (string "]\n") parseState
    <*> (parseOperation <* newline)
    <*> between (string "After:  [") (string "]\n") parseState

parseInput :: Parser Puzzle
parseInput =
  Puzzle
    <$> (parseSample `sepEndBy1` newline <* count 2 newline)
    <*> (parseOperation `sepEndBy1` newline)
    <* eof

part1 :: Puzzle -> IO ()
part1 (Puzzle samples _) =
  print
    . length
    . filter ((>= 3) . numInstructionMatch)
    $ samples
  where
    numInstructionMatch :: Sample -> Int
    numInstructionMatch sample = length $ filter (matchInstruction sample) $ M.elems allInstructions

part2 :: Puzzle -> IO ()
part2 (Puzzle samples operations) =
  print
    . reg0
    . foldl runOperation initState
    $ operations
  where
    initState = RegState 0 0 0 0

    instructionMap :: M.Map Int Instruction
    instructionMap = resolveInstructions samples

    runOperation :: RegState -> Operation -> RegState
    runOperation regState (Operation opCode a b c) = fromJust . (M.!) instructionMap opCode a b c $ regState
