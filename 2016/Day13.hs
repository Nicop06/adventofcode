module Day13
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (first, second)
import Data.Functor ((<&>))
import Data.Set qualified as S
import Text.Parsec
import Text.Parsec.String

type Position = (Int, Int)

isOpenSpace :: Int -> Position -> Bool
isOpenSpace n (x, y) = even $ numBits $ x * x + 3 * x + 2 * x * y + y + y * y + n

numBits :: Int -> Int
numBits 0 = 0
numBits n = n `rem` 2 + numBits (n `div` 2)

accessiblePositions :: Position -> [Position]
accessiblePositions p = filter isValidPos $ ([first, second] <*> [(+ 1), subtract 1]) <&> ($ p)
  where
    isValidPos (x, y) = x >= 0 && y >= 0

nextPositions :: Int -> S.Set Position -> S.Set Position
nextPositions n = S.fromList . filter (isOpenSpace n) . concatMap accessiblePositions . S.toList

allAccessiblePositions :: Int -> [S.Set Position]
allAccessiblePositions n = iterate (nextPositions n) (S.singleton (1, 1))

parseInput :: Parser Int
parseInput = read <$> many1 digit <* newline <* eof

part1 :: Int -> IO ()
part1 = print . length . takeWhile (not . ((31, 39) `S.member`)) . allAccessiblePositions

part2 :: Int -> IO ()
part2 = print . S.size . S.unions . take 51 . allAccessiblePositions
