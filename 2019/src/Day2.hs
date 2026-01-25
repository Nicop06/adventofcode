module Day2
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Monad.ST (ST)
import Data.Array.ST
import Data.Array.Unboxed
import Text.Parsec
import Text.Parsec.String

type Program = UArray Int Int

runProgram :: Int -> Int -> Program -> Int
runProgram noun verb p = (! 0) $ runSTUArray (start =<< thaw p)
  where
    op :: Int -> Int -> Int -> Int
    op 1 = (+)
    op 2 = (*)
    op c = error $ "Unknow code " ++ show c

    start :: STUArray s Int Int -> ST s (STUArray s Int Int)
    start a = do
      writeArray a 1 noun
      writeArray a 2 verb
      go 0 a

    go :: Int -> STUArray s Int Int -> ST s (STUArray s Int Int)
    go i a = do
      opCode <- readArray a i
      if opCode == 99
        then return a
        else do
          pos1 <- readArray a (i + 1)
          pos2 <- readArray a (i + 2)
          n1 <- readArray a pos1
          n2 <- readArray a pos2
          dest <- readArray a (i + 3)
          writeArray a dest (op opCode n1 n2)
          go (i + 4) a

toArray :: [Int] -> Program
toArray l = let len = length l in listArray (0, len - 1) l

parseInput :: Parser Program
parseInput =
  toArray
    <$> (read <$> many1 digit)
    `sepBy1` char ','
    <* newline
    <* eof

part1 :: Program -> IO ()
part1 = print . runProgram 12 2

part2 :: Program -> IO ()
part2 p = print $ go ((,) <$> inputs <*> inputs)
  where
    inputs :: [Int]
    inputs = [0 .. 99]

    go :: [(Int, Int)] -> Int
    go [] = error "No possible input found"
    go ((noun, verb) : rs) = case runProgram noun verb p of
      19690720 -> 100 * noun + verb
      _ -> go rs
