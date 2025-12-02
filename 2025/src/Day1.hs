module Day1
  ( parseInput
  , part1
  , part2
  , numPassByZero
  ) where

import Text.Parsec
import Text.Parsec.String

type Password = Int

data Turn
  = L
  | R
  deriving (Show, Eq, Read)

data Rotation =
  Rotation Turn Int
  deriving (Show, Eq)

numDials :: Int
numDials = 100

applyRotation :: Password -> Rotation -> Password
applyRotation p (Rotation R r) = p + r
applyRotation p (Rotation L r) = p - r

countZero :: [Password] -> Int
countZero = length . filter (== 0) . map (`rem` numDials)

numPassByZero :: Password -> Password -> Int
numPassByZero p1 p2
  | p1 < p2 = adjustedNumDivDiff (p1 + 1) p2
  | p2 < p1 = adjustedNumDivDiff (p2 - 1) (p1 - 1)
  | otherwise = 0
  where
    numDivDiff a b = (b `div` numDials) - (a `div` numDials)
    adjustedNumDivDiff a b
      | a <= 0 && b > 0 = numDivDiff (a - 1) b
      | a < 0 && b < 0 = numDivDiff (a - 1) (b - 1)
      | otherwise = numDivDiff a b

allNumPassByZero :: [Password] -> Int
allNumPassByZero (p1:p2:rs) = numPassByZero p1 p2 + allNumPassByZero (p2 : rs)
allNumPassByZero _ = 0

parseRotation :: Parser Rotation
parseRotation = Rotation . read . pure <$> anyChar <*> (read <$> many1 digit)

parseInput :: Parser [Rotation]
parseInput = parseRotation `sepEndBy1` newline <* eof

part1 :: [Rotation] -> IO ()
part1 = print . countZero . scanl applyRotation 50

part2 :: [Rotation] -> IO ()
part2 = print . allNumPassByZero . scanl applyRotation 50
