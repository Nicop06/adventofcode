module Day7
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array.Unboxed
import Data.List (permutations)
import Text.Parsec
import Text.Parsec.String

type Program = Array Int Int

runProgram :: Int -> Program -> [Int] -> [Int]
runProgram pos prog inputs = case opCode `mod` 100 of
  1 -> runProgram (pos + 4) (prog // [(getIm 3, get 1 + get 2)]) inputs
  2 -> runProgram (pos + 4) (prog // [(getIm 3, get 1 * get 2)]) inputs
  3 -> runProgram (pos + 2) (prog // [(getIm 1, head inputs)]) (drop 1 inputs)
  4 -> get 1 : runProgram (pos + 2) prog inputs
  5 -> runProgram (if get 1 /= 0 then get 2 else pos + 3) prog inputs
  6 -> runProgram (if get 1 == 0 then get 2 else pos + 3) prog inputs
  7 -> runProgram (pos + 4) (prog // [(getIm 3, fromEnum $ get 1 < get 2)]) inputs
  8 -> runProgram (pos + 4) (prog // [(getIm 3, fromEnum $ get 1 == get 2)]) inputs
  99 -> []
  _ -> error $ "Invalid opCode " ++ show opCode
  where
    opCode :: Int
    opCode = prog ! pos

    getIm :: Int -> Int
    getIm n = prog ! (pos + n)

    get :: Int -> Int
    get n = case opCode `mod` (10 ^ (n + 2)) `div` 10 ^ (n + 1) of
      0 -> prog ! getIm n
      1 -> getIm n
      f -> error $ "Invalid flag " ++ show f ++ " for arg " ++ show n

amplifier :: Program -> Int -> [Int] -> [Int]
amplifier ins phase inputs = runProgram 0 ins (phase : inputs)

parseInput :: Parser Program
parseInput =
  toArray
    <$> (read <$> many1 (digit <|> char '-'))
    `sepBy1` char ','
    <* newline
    <* eof
  where
    toArray :: [Int] -> Program
    toArray l = let len = length l in listArray (0, len - 1) l

part1 :: Program -> IO ()
part1 ins = print . maximum $ map runAmplifiers (permutations [0 .. 4])
  where
    runAmplifiers :: [Int] -> Int
    runAmplifiers = head . foldr (amplifier ins) [0]

part2 :: Program -> IO ()
part2 ins = print . maximum $ map feedbackLoop (permutations [5 .. 9])
  where
    feedbackLoop :: [Int] -> Int
    feedbackLoop phases = last output
      where
        output = foldr (amplifier ins) (0 : output) phases
