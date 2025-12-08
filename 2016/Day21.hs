module Day21
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array.Repa as A (Array, U, computeUnboxedP, extent, fromListUnboxed, toList, traverse)
import Data.Array.Repa.Index
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

type Size = Int

type Index = Int

type Letter = Char

type Password = Array U DIM1 Char

type UpdateFunc = Size -> (Index -> Letter) -> Index -> Letter

data Operation = SwapIndices Index Index | SwapLetters Letter Letter | RotateRight Index | RotateLeft Index | RotateLetter Letter | RotateBackLetter Letter | Reverse Index Index | Move Index Index deriving (Show, Eq)

initialPassword :: String -> Password
initialPassword p = fromListUnboxed (ix1 $ length p) p

applyUpdate :: UpdateFunc -> Password -> Password
applyUpdate update l = fromJust $ computeUnboxedP $ A.traverse l id updateFunc
  where
    updateFunc f = update (shapeToInt $ extent l) (f . ix1) . shapeToInt

operationFunc :: Operation -> UpdateFunc
operationFunc (SwapIndices i j) = const (swapIndicesFunc i j)
operationFunc (SwapLetters l l') = swapLettersFunc l l'
operationFunc (RotateLeft steps) = rotateFunc (-steps)
operationFunc (RotateRight steps) = rotateFunc steps
operationFunc (RotateLetter l) = rotateLetterFunc l
operationFunc (RotateBackLetter l) = rotateBackLetterFunc l
operationFunc (Reverse i j) = const (reverseFunc i j)
operationFunc (Move from to) = const (moveFunc from to)

unscrambleOperation :: Operation -> Operation
unscrambleOperation (RotateLeft i) = RotateRight i
unscrambleOperation (RotateRight i) = RotateLeft i
unscrambleOperation (RotateLetter l) = RotateBackLetter l
unscrambleOperation (Move from to) = Move to from
unscrambleOperation op = op

shapeToInt :: DIM1 -> Index
shapeToInt (Z :. i) = i

swapIndicesFunc :: Index -> Index -> (Index -> Letter) -> Index -> Letter
swapIndicesFunc i j f idx
  | i == idx = f j
  | j == idx = f i
  | otherwise = f idx

swapLettersFunc :: Letter -> Letter -> Size -> (Index -> Letter) -> Index -> Letter
swapLettersFunc l l' size f = swapIndicesFunc (letterIndex l size f) (letterIndex l' size f) f

rotateFunc :: Index -> Size -> (Index -> Letter) -> Index -> Letter
rotateFunc steps size f idx = f $ (idx - steps + size) `mod` size

rotateLetterFunc :: Letter -> Size -> (Index -> Letter) -> Index -> Letter
rotateLetterFunc l size f = rotateFunc (numStepsForLetter l size f) size f

rotateBackLetterFunc :: Letter -> Size -> (Index -> Letter) -> Index -> Letter
rotateBackLetterFunc l size f =
  let steps = snd . head . filter ((== letterIndex l size f) . (`mod` size) . uncurry (+)) . zip indices $ map (numStepsForLetterIndex size) indices
   in rotateFunc (-steps) size f
  where
    indices = [0 .. size - 1]

numStepsForLetterIndex :: Size -> Index -> Index
numStepsForLetterIndex size i = (i + (if abs i >= 4 then 2 else 1) + size) `mod` size

numStepsForLetter :: Letter -> Size -> (Index -> Letter) -> Index
numStepsForLetter l size f = let i = letterIndex l size f in numStepsForLetterIndex size i

reverseFunc :: Index -> Index -> (Index -> Letter) -> Index -> Letter
reverseFunc i j f idx
  | idx <= j && idx >= i = f $ j + i - idx
  | otherwise = f idx

moveFunc :: Index -> Index -> (Index -> Letter) -> Index -> Letter
moveFunc from to f idx
  | idx < min from to || idx > max from to = f idx
  | idx == to = f from
  | otherwise = if from > to then f (idx - 1) else f (idx + 1)

letterIndex :: Letter -> Size -> (Index -> Letter) -> Index
letterIndex l size f = head . filter ((== l) . f) $ [0 .. (size - 1)]

applyOperation :: Operation -> Password -> Password
applyOperation = applyUpdate . operationFunc

runAllOperations :: String -> [Operation] -> [String]
runAllOperations p = map toList . scanl (flip applyOperation) (initialPassword p)

parseInput :: Parser [Operation]
parseInput = parseOperation `sepEndBy1` newline <* eof

parseOperation :: Parser Operation
parseOperation = choice [parseSwap, try parseRotate, try parseReverse, parseMove]

parseSwap :: Parser Operation
parseSwap = string "swap " *> choice [parseSwapIndices, parseSwapLetters]
  where
    parseSwapIndices = SwapIndices <$> (string "position " *> parseNum) <*> (string " with position " *> parseNum)
    parseSwapLetters = SwapLetters <$> (string "letter " *> alphaNum) <*> (string " with letter " *> alphaNum)

parseRotate :: Parser Operation
parseRotate = string "rotate " *> choice [parseRotateLeft, rotateRight, parseRotateLetter]
  where
    parseRotateLeft = RotateLeft <$> (string "left " *> parseNum <* string " step" <* optional (char 's'))
    rotateRight = RotateRight <$> (string "right " *> parseNum <* string " step" <* optional (char 's'))
    parseRotateLetter = RotateLetter <$> (string "based on position of letter " *> alphaNum)

parseReverse :: Parser Operation
parseReverse = Reverse <$> (string "reverse positions " *> parseNum <* string " through ") <*> parseNum

parseMove :: Parser Operation
parseMove = Move <$> (string "move position " *> parseNum <* string " to position ") <*> parseNum

parseNum :: Parser Int
parseNum = read <$> many1 digit

part1 :: [Operation] -> IO ()
part1 = putStrLn . last . runAllOperations "abcdefgh"

part2 :: [Operation] -> IO ()
part2 = putStrLn . last . runAllOperations "fbgdceah" . map unscrambleOperation . reverse
