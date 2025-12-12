module Day10
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (first)
import Data.Array
import Debug.Trace
import Text.Parsec
import Text.Parsec.String

data LightState = Off | On deriving (Eq, Show, Enum)

type MachineState = Array Int LightState

type JoltageMatrix = Array (Int, Int) Int

newtype Button = Button [Int] deriving (Eq, Show)

type Joltage = Array Int Int

data Machine = Machine {getLightState :: MachineState, getButtons :: [Button], getJoltage :: Joltage} deriving (Eq, Show)

------------
-- Lights --
------------

listToArray :: [a] -> Array Int a
listToArray l = let len = length l in listArray (0, len - 1) l

combinationsOf :: Int -> [a] -> [[a]]
combinationsOf numElems els = go numElems els
  where
    go 0 _ = []
    go _ [] = []
    go 1 (x : xs) = [x] : go 1 xs
    go n (x : xs) = map (x :) (go (n - 1) els) ++ go n xs

toggleLight :: LightState -> LightState
toggleLight On = Off
toggleLight Off = On

updateLights :: Button -> MachineState -> MachineState
updateLights (Button idx) machineState = machineState // [(i, toggleLight (machineState ! i)) | i <- idx]

hasAny :: [a] -> Bool
hasAny [] = False
hasAny _ = True

numButtonsNeededForLightState :: Machine -> Int
numButtonsNeededForLightState (Machine state buttons _) = go 1
  where
    canMatchState :: Int -> Bool
    canMatchState numButtons =
      hasAny $
        filter (== state) $
          map (foldr updateLights (listToArray $ replicate (length state) Off)) $
            combinationsOf numButtons buttons
    go :: Int -> Int
    go n
      | canMatchState n = n
      | otherwise = go (n + 1)

-------------
-- Joltage --
-------------

makeJoltageMatrix :: Machine -> JoltageMatrix
makeJoltageMatrix (Machine _ buttons joltage) =
  let (_, m) = bounds joltage
      (n, idx) = buttonIndices 0 buttons
      b = ((0, 0), (m, n))
   in array b [(i, 0) | i <- range b] // map (,1) idx // map (first (,n)) (assocs joltage)
  where
    buttonIndices :: Int -> [Button] -> (Int, [(Int, Int)])
    buttonIndices i [] = (i, [])
    buttonIndices i (Button idx : bs) =
      let (n, idx') = buttonIndices (i + 1) bs
       in (n, idx' ++ [(j, i) | j <- idx])

copyRow :: Int -> Int -> JoltageMatrix -> [((Int, Int), Int)]
copyRow fromRow toRow m =
  let ((_, startCol), (_, endCol)) = bounds m
   in [((toRow, j), m ! (fromRow, j)) | j <- range (startCol, endCol)]

swapRows :: Int -> Int -> JoltageMatrix -> JoltageMatrix
swapRows i i' m = m // copyRow i i' m // copyRow i' i m

debug = flip trace

subRow :: Int -> Int -> Int -> Int -> JoltageMatrix -> JoltageMatrix
subRow startRow endRow row col m =
  let ((_, startCol), (_, endCol)) = bounds m
   in (m // [((i, j), (m ! (i, j)) * m ! (row, col) - m ! (row, j) * m ! (i, col)) | (i, j) <- range ((startRow, startCol), (endRow, endCol))])

triangulariseMatrix :: JoltageMatrix -> JoltageMatrix
triangulariseMatrix joltageMatrix = go startRow startCol joltageMatrix
  where
    ((startRow, startCol), (endRow, endCol)) = bounds joltageMatrix
    go :: Int -> Int -> JoltageMatrix -> JoltageMatrix
    go row col m
      | row > endRow || col >= endCol = m
      | otherwise =
          let rowsToSelect = filter ((/= 0) . snd) [(i, m ! (i, col)) | i <- [row .. endRow]]
           in case rowsToSelect of
                [] -> go row (col + 1) m
                ((newRow, _) : _) ->
                  let m' = swapRows row newRow m
                   in go (row + 1) (col + 1) (subRow (row + 1) endRow row col m')

removeEmptyRows :: JoltageMatrix -> JoltageMatrix
removeEmptyRows joltageMatrix =
  let lastRow = go startRow
   in array ((startRow, startCol), (lastRow, endCol)) [((i, j), joltageMatrix ! (i, j)) | (i, j) <- range b, i <= lastRow]
  where
    b@((startRow, startCol), (endRow, endCol)) = bounds joltageMatrix
    go :: Int -> Int
    go i
      | isRowEmpty i = i - 1
      | i == endRow = endRow
      | otherwise = go (i + 1)
    isRowEmpty :: Int -> Bool
    isRowEmpty i = all (== 0) [joltageMatrix ! (i, j) | j <- range (startCol, endCol)]

-- Assumes that the matrix is triangular
diagonaliseMatrix :: JoltageMatrix -> JoltageMatrix
diagonaliseMatrix joltageMatrix = go endRow joltageMatrix
  where
    ((startRow, startCol), (endRow, endCol)) = bounds joltageMatrix
    go :: Int -> JoltageMatrix -> JoltageMatrix
    go 0 m = m
    go row m = case [j | j <- range (startCol, endCol), m ! (row, j) /= 0] of
      [] -> go (row - 1) m
      j : _ -> let m' = subRow startRow (row - 1) row j m in go (row - 1) m'

reduceMatrix :: JoltageMatrix -> JoltageMatrix
reduceMatrix = diagonaliseMatrix . removeEmptyRows . triangulariseMatrix

printMatrix :: JoltageMatrix -> [String]
printMatrix m = let ((startRow, startCol), (endRow, endCol)) = bounds m in [] : [show [m ! (i, j) | j <- range (startCol, endCol)] | i <- range (startRow, endRow)]

------------
-- Parser --
------------

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseButton :: Parser Button
parseButton = Button <$> between (char '(') (char ')') (parseNumber `sepBy1` char ',')

parseJoltage :: Parser Joltage
parseJoltage = listToArray <$> between (char '{') (char '}') (parseNumber `sepBy` char ',')

parseLightState :: Parser LightState
parseLightState = Off <$ char '.' <|> On <$ char '#'

parseMachineState :: Parser MachineState
parseMachineState = listToArray <$> between (char '[') (char ']') (many1 parseLightState)

parseMachine :: Parser Machine
parseMachine = Machine <$> parseMachineState <* char ' ' <*> (parseButton `endBy1` char ' ') <*> parseJoltage

parseInput :: Parser [Machine]
parseInput = parseMachine `sepEndBy1` newline <* eof

part1 :: [Machine] -> IO ()
part1 = print . sum . map numButtonsNeededForLightState

part2 :: [Machine] -> IO ()
part2 = mapM_ (mapM_ putStrLn . printMatrix . reduceMatrix . makeJoltageMatrix) . take 10
