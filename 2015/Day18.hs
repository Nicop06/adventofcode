module Day18
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array.Repa as A
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

-- Data

type LightState = Bool

type Lights = Array U DIM2 LightState

type UpdateLightState = DIM2 -> (DIM2 -> LightState) -> DIM2 -> LightState

-- Helpers

repeatUpdate :: UpdateLightState -> Int -> Lights -> Lights
repeatUpdate update n = (!! n) . iterate (updateLights update)

updateLights :: UpdateLightState -> Lights -> Lights
updateLights update l = fromJust $ computeUnboxedP $ A.traverse l id (update $ extent l)

newLightState :: UpdateLightState
newLightState s f idx =
  if f idx
    then numNeighborsOn == 2 || numNeighborsOn == 3
    else numNeighborsOn == 3
  where
    numNeighborsOn = length . filter f . allNeighbors s $ idx

newLightStateWithBrokenCorners :: UpdateLightState
newLightStateWithBrokenCorners s f idx
  | isCorner s idx = True
  | otherwise = newLightState s f idx

turnCornersOn :: Lights -> Lights
turnCornersOn = updateLights (\s f idx -> isCorner s idx || f idx)

isCorner :: DIM2 -> DIM2 -> Bool
isCorner (Z :. r :. c) (Z :. i :. j) =
  i == 0 && j == 0 || i == 0 && j == c - 1 || i == r - 1 && j == 0 || i == r - 1 && j == r - 1

allNeighbors :: DIM2 -> DIM2 -> [DIM2]
allNeighbors (Z :. r :. c) (Z :. i :. j) =
  uncurry ix2 <$> filter isValid possibleNeighbors
  where
    possibleNeighbors = [(i', j') | i' <- [i - 1 .. i + 1], j' <- [j - 1 .. j + 1]]
    isValid (i', j') = i' >= 0 && j' >= 0 && i' < r && j' < c && (i', j') /= (i, j)

numLightsOn :: Lights -> Int
numLightsOn = length . filter id . toList

-- Parser

parseLight :: Parser LightState
parseLight = (True <$ char '#') <|> (False <$ char '.')

listToArray :: [[LightState]] -> Lights
listToArray l = fromListUnboxed (ix2 (length l) (length $ head l)) $ concat l

parseInput :: Parser Lights
parseInput = listToArray <$> (many1 parseLight `sepEndBy1` newline <* eof)

part1 :: Lights -> IO ()
part1 = print . numLightsOn . repeatUpdate newLightState 100

part2 :: Lights -> IO ()
part2 = print . numLightsOn . repeatUpdate newLightStateWithBrokenCorners 100 . turnCornersOn
