module Day21
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array.Repa as A (Array, U, computeUnboxedP, extent, fromListUnboxed, toList, traverse)
import Data.Array.Repa.Index
import Data.List qualified as L
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

type Size = Int

type Index = Int

type Letter = Char

type Password = Array U DIM1 Char

data Position = Index Index | Letter Letter deriving (Show, Eq)

type UpdateFunc = Size -> (Index -> Letter) -> Index -> Letter

data Operation = Swap Position Position | Rotate Position | Reverse Index Index | Move Index Index deriving (Show, Eq)

initialPassword :: String -> Password
initialPassword p = fromListUnboxed (ix1 $ length p) p

applyUpdate :: UpdateFunc -> Password -> Password
applyUpdate update l = fromJust $ computeUnboxedP $ A.traverse l id updateFunc
  where
    updateFunc f = update (shapeToInt $ extent l) (f . ix1) . shapeToInt

operationFunc :: Operation -> UpdateFunc
operationFunc (Swap pos pos') size = swapFunc pos pos' size
operationFunc (Rotate steps) size = rotateFunc steps size
operationFunc (Reverse i j) _ = reverseFunc i j
operationFunc (Move from to) _ = moveFunc from to

shapeToInt :: DIM1 -> Index
shapeToInt (Z :. i) = i

swapFunc :: Position -> Position -> Size -> (Index -> Letter) -> Index -> Letter
swapFunc pos pos' size f idx
  | i == idx = f j
  | j == idx = f i
  | otherwise = f idx
  where
    i = indexFromPosition pos size f
    j = indexFromPosition pos' size f

rotateFunc :: Position -> Size -> (Index -> Letter) -> Index -> Letter
rotateFunc pos size f idx = f $ (idx - steps + size) `mod` size
  where
    numSteps (Index i) = i
    numSteps (Letter _) =
      let i = indexFromPosition pos size f
       in i + (if abs i >= 4 then 2 else 1)
    steps = numSteps pos

reverseFunc :: Index -> Index -> (Index -> Letter) -> Index -> Letter
reverseFunc i j f idx
  | idx <= j && idx >= i = f $ j + i - idx
  | otherwise = f idx

moveFunc :: Index -> Index -> (Index -> Letter) -> Index -> Letter
moveFunc from to f idx
  | idx < min from to || idx > max from to = f idx
  | idx == to = f from
  | otherwise = if from > to then f (idx - 1) else f (idx + 1)

indexFromPosition :: Position -> Size -> (Index -> Letter) -> Index
indexFromPosition (Index idx) _ _ = idx
indexFromPosition (Letter l) size f = head . filter ((== l) . f) $ [0 .. (size - 1)]

applyOperation :: Operation -> Password -> Password
applyOperation = applyUpdate . operationFunc

runAllOperations :: String -> [Operation] -> [String]
runAllOperations p = map toList . L.scanl (flip applyOperation) (initialPassword p)

parseInput :: Parser [Operation]
parseInput = parseOperation `sepEndBy1` newline <* eof

parseOperation :: Parser Operation
parseOperation = choice [parseSwap, try parseRotate, try parseReverse, parseMove]

parseSwap :: Parser Operation
parseSwap = string "swap " *> choice [swapIndex, swapLetter]
  where
    swapIndex = Swap <$> (string "position " *> parseIndex) <*> (string " with position " *> parseIndex)
    swapLetter = Swap <$> (string "letter " *> parseLetter) <*> (string " with letter " *> parseLetter)

parseRotate :: Parser Operation
parseRotate = string "rotate " *> choice [rotateLeft, rotateRight, rotateLetter]
  where
    rotateLeft = Rotate <$> (string "left " *> (Index . negate <$> parseNum) <* string " step" <* optional (char 's'))
    rotateRight = Rotate <$> (string "right " *> parseIndex <* string " step" <* optional (char 's'))
    rotateLetter = Rotate <$> (string "based on position of letter " *> parseLetter)

parseReverse :: Parser Operation
parseReverse = Reverse <$> (string "reverse positions " *> parseNum <* string " through ") <*> parseNum

parseMove :: Parser Operation
parseMove = Move <$> (string "move position " *> parseNum <* string " to position ") <*> parseNum

parseIndex :: Parser Position
parseIndex = Index <$> parseNum

parseLetter :: Parser Position
parseLetter = Letter <$> alphaNum

parseNum :: Parser Int
parseNum = read <$> many1 digit

part1 :: [Operation] -> IO ()
part1 = putStrLn . last . runAllOperations "abcdefgh"

part2 :: [Operation] -> IO ()
part2 = mapM_ print
