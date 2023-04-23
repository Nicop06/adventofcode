module Day1
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (partition)
import Text.Parsec
import Text.Parsec.String

type Coordinates = (Int, Int)

data Turn = L | R deriving (Show, Eq, Read)

data Instruction = Instruction Turn Int deriving (Show, Eq)

data Direction = N | E | S | W deriving (Show, Eq, Enum)

data MyState = MyState {getCoordinates :: Coordinates, getDirection :: Direction} deriving (Show, Eq)

turn :: Turn -> MyState -> MyState
turn R s@(MyState _ d) = s {getDirection = turnRight d}
  where
    turnRight W = N
    turnRight d' = succ d'
turn L s@(MyState _ d) = s {getDirection = turnLeft d}
  where
    turnLeft N = W
    turnLeft d' = pred d'

move :: Int -> MyState -> MyState
move n s@(MyState (x, y) d) = s {getCoordinates = updateCoord d}
  where
    updateCoord N = (x, y + n)
    updateCoord S = (x, y - n)
    updateCoord E = (x + n, y)
    updateCoord W = (x - n, y)

nextState :: Instruction -> MyState -> MyState
nextState (Instruction t n) = move n . turn t

followInstructions :: [Instruction] -> [MyState]
followInstructions = scanl (flip nextState) initialState
  where
    initialState = MyState (0, 0) N

distance :: Coordinates -> Int
distance (x, y) = abs x + abs y

allCoordinates :: [MyState] -> [Coordinates]
allCoordinates = allPositions . map getCoordinates
  where
    allPositions [] = []
    allPositions [c] = [c]
    allPositions (c : c' : xs) = init (enumerateCoordinates c c') ++ allPositions (c' : xs)

enumerateCoordinates :: Coordinates -> Coordinates -> [Coordinates]
enumerateCoordinates (x, y) (x', y')
  | x == x' = (x,) <$> range y y'
  | y == y' = (,y) <$> range x x'
  | otherwise = error "At least one coordinate must be equal"
  where
    range a b = if a < b then [a .. b] else reverse [b .. a]

listDups :: Eq a => [a] -> [a]
listDups [] = []
listDups (x : xs) =
  let (group, xs') = partition (== x) xs
   in if null group then listDups xs' else x : listDups xs'

parseInstruction :: Parser Instruction
parseInstruction = Instruction <$> (read . pure <$> anyChar) <*> (read <$> many1 digit)

parseInput :: Parser [Instruction]
parseInput = parseInstruction `sepBy1` string ", "

part1 :: [Instruction] -> IO ()
part1 = print . distance . getCoordinates . last . followInstructions

part2 :: [Instruction] -> IO ()
part2 = print . distance . head . listDups . allCoordinates . followInstructions
