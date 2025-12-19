module Day21
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

type Size = Int

type Index = Int

type Letter = Char

type Password = String

data Operation
  = SwapIndices Index Index
  | SwapLetters Letter Letter
  | RotateRight Index
  | RotateLeft Index
  | RotateLetter Letter
  | RotateBackLetter Letter
  | Reverse Index Index
  | Move Index Index
  deriving (Show, Eq)

applyOperation :: Operation -> Password -> Password
applyOperation (SwapIndices i j) p = swapLetters (p !! i) (p !! j) p
applyOperation (SwapLetters l l') p = swapLetters l l' p
applyOperation (RotateLeft steps) p = rotate steps p
applyOperation (RotateRight steps) p = rotate (length p - steps) p
applyOperation (RotateLetter l) p = rotate (length p - numStepsForLetter l p) p
applyOperation (RotateBackLetter l) p = rotateBackLetter l p
applyOperation (Reverse i j) p = reverseBetween i j p
applyOperation (Move from to) p = move from to p

unscrambleOperation :: Operation -> Operation
unscrambleOperation (RotateLeft i) = RotateRight i
unscrambleOperation (RotateRight i) = RotateLeft i
unscrambleOperation (RotateLetter l) = RotateBackLetter l
unscrambleOperation (Move from to) = Move to from
unscrambleOperation op = op

swapLetters :: Letter -> Letter -> Password -> Password
swapLetters l l' = map swap
  where
    swap p
      | p == l = l'
      | p == l' = l
      | otherwise = p

rotate :: Index -> Password -> Password
rotate steps p = let (p1, p2) = splitAt (steps `mod` length p) p in p2 ++ p1

rotateBackLetter :: Letter -> Password -> Password
rotateBackLetter l p =
  let originalIndex = head $ filter rotationMatches [0 .. size - 1]
   in rotate (numStepsForLetterIndex size originalIndex) p
  where
    rotationMatches i = (i + numStepsForLetterIndex size i) `mod` size == letterIndex
    letterIndex = fromJust $ elemIndex l p
    size = length p

numStepsForLetterIndex :: Size -> Index -> Index
numStepsForLetterIndex size i = (i + (if abs i >= 4 then 2 else 1) + size) `mod` size

numStepsForLetter :: Letter -> Password -> Index
numStepsForLetter l p = numStepsForLetterIndex (length p) (fromJust $ elemIndex l p)

reverseBetween :: Index -> Index -> Password -> Password
reverseBetween i j p
  | j < i = reverseBetween j i p
  | otherwise =
      let (start, rs) = splitAt i p
          (mid, end) = splitAt (j - i + 1) rs
       in start ++ reverse mid ++ end

move :: Index -> Index -> Password -> Password
move from to p =
  case splitAt from p of
    (_, []) -> p
    (left, l : right) ->
      if to < from
        then let (start, mid) = splitAt to left in start ++ [l] ++ mid ++ right
        else let (mid, end) = splitAt (to - from) right in left ++ mid ++ [l] ++ end

runAllOperations :: String -> [Operation] -> String
runAllOperations = foldl (flip applyOperation)

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
part1 = putStrLn . runAllOperations "abcdefgh"

part2 :: [Operation] -> IO ()
part2 = putStrLn . runAllOperations "fbgdceah" . map unscrambleOperation . reverse
