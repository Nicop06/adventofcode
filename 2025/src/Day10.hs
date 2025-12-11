module Day10
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array
import Data.List (permutations)
import Data.Maybe (mapMaybe)
import Text.Parsec
import Text.Parsec.String

data LightState = Off | On deriving (Eq, Show, Enum)

type MachineState = Array Int LightState

newtype Button = Button [Int] deriving (Eq, Show)

type Joltage = Array Int Int

data Machine = Machine {getLightState :: MachineState, getButtons :: [Button], getJoltage :: Joltage} deriving (Eq, Show)

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

numButtonsNeededForJoltage :: Machine -> Int
numButtonsNeededForJoltage (Machine _ buttons joltage) = case mapMaybe (go joltage) (permutations buttons) of
  [] -> 0
  l -> minimum l
  where
    go :: Joltage -> [Button] -> Maybe Int
    go _ [] = Nothing
    go j (Button idx : bs) =
      let n = minimum [j ! i | i <- idx]
          j' = j // [(i, (j ! i) - n) | i <- idx]
       in go' j' n
      where
        go' :: Joltage -> Int -> Maybe Int
        go' j' n
          | all (== 0) (elems j') = Just n
          | any (< 0) (elems j') = Nothing
          | otherwise = (+ n) <$> go j' bs

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
part2 = print . sum . map numButtonsNeededForJoltage
