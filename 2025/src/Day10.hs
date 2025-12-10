module Day10
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array
import Text.Parsec
import Text.Parsec.String

data LightState = Off | On deriving (Eq, Show, Enum)

type MachineState = Array Int LightState

newtype Button = Button [Int] deriving (Eq, Show)

newtype Joltage = Joltage [Int] deriving (Eq, Show)

data Machine = Machine {getLightState :: MachineState, getButtons :: [Button], getJoltage :: Joltage} deriving (Eq, Show)

makeMachineState :: [LightState] -> MachineState
makeMachineState lights = let len = length lights in listArray (0, len - 1) lights

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

applyButton :: Button -> MachineState -> MachineState
applyButton (Button idx) machineState = machineState // [(i, toggleLight (machineState ! i)) | i <- idx]

applyButtons :: Int -> [Button] -> MachineState
applyButtons n = foldr applyButton (makeMachineState $ replicate n Off)

hasAny :: [a] -> Bool
hasAny [] = False
hasAny _ = True

canMatchState :: MachineState -> [Button] -> Int -> Bool
canMatchState m b n = hasAny $ filter (== m) $ map (applyButtons (length m)) (combinationsOf n b)

numButtonsNeeded :: Machine -> Int
numButtonsNeeded (Machine state buttons _) = go 1
  where
    go :: Int -> Int
    go n
      | canMatchState state buttons n = n
      | otherwise = go (n + 1)

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseButton :: Parser Button
parseButton = Button <$> between (char '(') (char ')') (parseNumber `sepBy1` char ',')

parseJoltage :: Parser Joltage
parseJoltage = Joltage <$> between (char '{') (char '}') (parseNumber `sepBy` char ',')

parseLightState :: Parser LightState
parseLightState = Off <$ char '.' <|> On <$ char '#'

parseMachineState :: Parser MachineState
parseMachineState = makeMachineState <$> between (char '[') (char ']') (many1 parseLightState)

parseMachine :: Parser Machine
parseMachine = Machine <$> parseMachineState <* char ' ' <*> (parseButton `endBy1` char ' ') <*> parseJoltage

parseInput :: Parser [Machine]
parseInput = parseMachine `sepEndBy1` newline <* eof

part1 :: [Machine] -> IO ()
part1 = print . sum . map numButtonsNeeded

part2 :: [Machine] -> IO ()
part2 = print . const 10
