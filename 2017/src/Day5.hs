module Day5
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Text.Parsec
import Text.Parsec.String

numSteps :: (Int -> Int) -> [Int] -> Int
numSteps updateFn ins = runST $ V.thaw (V.fromList ins) >>= go 0 0
  where
    go pos n v
      | pos >= 0 && pos < length ins = do
          offset <- M.read v pos
          M.modify v updateFn pos
          go (pos + offset) (n + 1) v
      | otherwise = return n

parseInput :: Parser [Int]
parseInput = (read <$> many1 (digit <|> char '-')) `sepEndBy1` newline <* eof

part1 :: [Int] -> IO ()
part1 = print . numSteps (+ 1)

part2 :: [Int] -> IO ()
part2 = print . numSteps updateFn
  where
    updateFn i
      | i >= 3 = i - 1
      | otherwise = i + 1
