module IntCode
  ( runProgram,
    parseProgram,
  )
where

import qualified Data.IntMap as M
import Text.Parsec
import Text.Parsec.String

type Memory = M.IntMap Int

data ProgramState = ProgramState
  { _pos :: Int,
    _relBase :: Int,
    _mem :: Memory,
    _inputs :: [Int]
  }
  deriving (Show)

runProgram :: [Int] -> [Int] -> [Int]
runProgram prog = run . ProgramState 0 0 (M.fromList $ zip [0 ..] prog)

run :: ProgramState -> [Int]
run st@(ProgramState pos relBase mem inputs) = case opCode `mod` 100 of
  1 -> run . moveBy 4 . update (getDest 3) (getArg 1 + getArg 2) $ st
  2 -> run . moveBy 4 . update (getDest 3) (getArg 1 * getArg 2) $ st
  3 -> run . moveBy 2 . update (getDest 1) (head inputs) $ st {_inputs = tail inputs}
  4 -> getArg 1 : run (moveBy 2 st)
  5 -> run . moveTo (if getArg 1 /= 0 then getArg 2 else pos + 3) $ st
  6 -> run . moveTo (if getArg 1 == 0 then getArg 2 else pos + 3) $ st
  7 -> run . moveBy 4 . update (getDest 3) (fromEnum $ getArg 1 < getArg 2) $ st
  8 -> run . moveBy 4 . update (getDest 3) (fromEnum $ getArg 1 == getArg 2) $ st
  9 -> run . moveBy 2 $ st {_relBase = relBase + getArg 1}
  99 -> []
  _ -> error $ "Invalid opCode " ++ show opCode
  where
    opCode :: Int
    opCode = case M.lookup pos mem of
      Nothing -> error $ "Invalid position " ++ show pos
      Just o -> o

    moveTo :: Int -> ProgramState -> ProgramState
    moveTo n s = s {_pos = n}

    moveBy :: Int -> ProgramState -> ProgramState
    moveBy n = moveTo (pos + n)

    update :: Int -> Int -> ProgramState -> ProgramState
    update addr val s = s {_mem = M.insert addr val mem}

    get :: Int -> Int
    get n = M.findWithDefault 0 n mem

    getIm :: Int -> Int
    getIm n = get (pos + n)

    flag :: Int -> Int
    flag n = opCode `mod` (10 ^ (n + 2)) `div` 10 ^ (n + 1)

    getArg :: Int -> Int
    getArg n = case flag n of
      0 -> get (getIm n)
      1 -> getIm n
      2 -> get (relBase + getIm n)
      f -> error $ "Invalid position flag " ++ show f ++ " for arg " ++ show n

    getDest :: Int -> Int
    getDest n = case flag n of
      0 -> getIm n
      2 -> relBase + getIm n
      f -> error $ "Invalid destination flag " ++ show f ++ " for arg " ++ show n

parseProgram :: Parser [Int]
parseProgram =
  (read <$> many1 (digit <|> char '-'))
    `sepBy1` char ','
    <* newline
    <* eof
