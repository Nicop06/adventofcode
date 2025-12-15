module Day10
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array
import Data.SBV
import Data.SBV.Dynamic
import Data.SBV.Internals (SMTModel (modelObjectives))
import Text.Parsec
import Text.Parsec.String

data LightState = Off | On deriving (Eq, Show, Enum)

type MachineState = Array Int LightState

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

joltageProblem :: Machine -> IO OptimizeResult
joltageProblem (Machine _ buttons joltage) = optimize Lexicographic $ do
  variables <- mapM (\i -> sInteger $ "b" ++ show i) [1 .. length buttons]
  mapM_ (\var -> constrain $ var .>= 0) variables
  mapM_ (makeJoltageConstrains variables) (range $ bounds joltage)
  minimize "goal" $ sum variables
  where
    makeJoltageConstrains :: [SInteger] -> Int -> Symbolic ()
    makeJoltageConstrains variables i =
      let buttonVars = map (\(_, v) -> variables !! v) . filter (\(Button idx, _) -> i `elem` idx) $ zip buttons [0 ..]
       in constrain $ sum buttonVars .== literal (toInteger (joltage ! i))

getResult :: OptimizeResult -> Integer
getResult (LexicographicResult (Satisfiable _ result)) =
  let objectives = modelObjectives result
   in case objectives of
        [(_, RegularCV res)] ->
          let value = cvVal res
           in case value of
                CInteger v -> v
                _ -> error "Unknown value type"
        _ -> error "Unknown objective types"
getResult _ = error "Unknown result type"

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
part2 machines = do
  allResults <- mapM joltageProblem machines
  print $ sum $ map getResult allResults
