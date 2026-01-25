{-# LANGUAGE RankNTypes #-}

module Day5
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Foldable
import Data.Sequence as S (Seq, empty, (<|))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Parsec hiding (State)
import Text.Parsec.String

type Program s = MV.MVector s Int

data State s = State
  { _prog :: Program s,
    _ip :: Int,
    _input :: [Int],
    _output :: Seq Int
  }

type Operation s = State s -> ST s (Maybe (State s))

type Get s = Program s -> Int -> ST s Int

data Flag = POS | IM deriving (Enum, Show)

type Flags = (Flag, Flag, Flag)

getAt :: Get s
getAt p i = MV.read p i >>= MV.read p

get :: Flag -> Get s
get POS = getAt
get IM = MV.read

readFlags :: Int -> Flags
readFlags f =
  ( toEnum (getDigit 2),
    toEnum (getDigit 3),
    toEnum (getDigit 4)
  )
  where
    getDigit :: Int -> Int
    getDigit d = (f `mod` (10 ^ (d + 1))) `div` (10 ^ d)

combineOp :: forall s. (Int -> Int -> Int) -> Flags -> Operation s
combineOp op (f1, f2, _) st@(State p i _ _) = do
  n1 <- get f1 p (i + 1)
  n2 <- get f2 p (i + 2)
  dest <- MV.read p (i + 3)
  MV.write p dest (op n1 n2)
  return $ Just st {_ip = i + 4}

output :: Flags -> Operation s
output (f1, _, _) st@(State p i _ o) = do
  n <- get f1 p (i + 1)
  return $ Just st {_ip = i + 2, _output = n <| o}

input :: Flags -> Operation s
input _ st@(State p ip inputs _) = case inputs of
  [] -> error "No more input to consume"
  (i : is) -> do
    dest <- MV.read p (ip + 1)
    MV.write p dest i
    return $ Just st {_ip = ip + 2, _input = is}

jumpIf :: (Int -> Bool) -> Flags -> Operation s
jumpIf cond (f1, f2, _) st@(State p ip _ _) = do
  val <- get f1 p (ip + 1)
  addr <- get f2 p (ip + 2)
  return $ Just st {_ip = if cond val then addr else ip + 3}

comp :: (Int -> Int -> Bool) -> Flags -> Operation s
comp cond (f1, f2, _) st@(State p ip _ _) = do
  val1 <- get f1 p (ip + 1)
  val2 <- get f2 p (ip + 2)
  dest <- MV.read p (ip + 3)
  MV.write p dest (fromEnum $ cond val1 val2)
  return $ Just st {_ip = ip + 4}

operation :: Int -> Flags -> Operation s
operation 1 = combineOp (+)
operation 2 = combineOp (*)
operation 3 = input
operation 4 = output
operation 5 = jumpIf (/= 0)
operation 6 = jumpIf (== 0)
operation 7 = comp (<)
operation 8 = comp (==)
operation 99 = const . const (return Nothing)
operation c = error $ "Unknow code " ++ show c

runProgram :: [Int] -> [Int] -> [Int]
runProgram inputs instructions = runST start
  where
    start :: ST s [Int]
    start = do
      p <- V.thaw $ V.fromList instructions
      reverse . toList <$> run (State p 0 inputs S.empty)

    run :: State s -> ST s (Seq Int)
    run st@(State p i _ _) = do
      opCode <- MV.read p i
      nextSt <- operation (opCode `mod` 100) (readFlags opCode) st
      case nextSt of
        Nothing -> return $ _output st
        Just next -> run next

parseInput :: Parser [Int]
parseInput =
  (read <$> many1 (digit <|> char '-'))
    `sepBy1` char ','
    <* newline
    <* eof

part1 :: [Int] -> IO ()
part1 = print . last . runProgram [1]

part2 :: [Int] -> IO ()
part2 = print . head . runProgram [5]
