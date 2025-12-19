module Day18
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.Array
import Text.Parsec
import Text.Parsec.String

-- Data

type Coord = (Int, Int)

type Bounds = (Coord, Coord)

type LightState = Bool

type Lights = Array Coord LightState

type UpdateLightState = Lights -> Coord -> LightState

-- Helpers

repeatUpdate :: UpdateLightState -> Int -> Lights -> Lights
repeatUpdate update n = (!! n) . iterate (updateLights update)

updateLights :: UpdateLightState -> Lights -> Lights
updateLights update lights = lights // [(c, update lights c) | c <- range (bounds lights)]

newLightState :: Lights -> Coord -> LightState
newLightState lights c =
  if lights ! c
    then numNeighborsOn == 2 || numNeighborsOn == 3
    else numNeighborsOn == 3
  where
    numNeighborsOn = length . filter (lights !) . allNeighbors (bounds lights) $ c

newLightStateWithBrokenCorners :: Lights -> Coord -> LightState
newLightStateWithBrokenCorners lights c
  | c `elem` corners lights = True
  | otherwise = newLightState lights c

turnCornersOn :: Lights -> Lights
turnCornersOn lights = lights // map (,True) (corners lights)

corners :: Lights -> [Coord]
corners lights = let ((minX, minY), (maxX, maxY)) = bounds lights in [(minX,), (maxX,)] <*> [minY, maxY]

allNeighbors :: Bounds -> Coord -> [Coord]
allNeighbors b c = filter (inRange b) possibleNeighbors
  where
    transforms = [(+ 1), id, flip (-) 1]
    possibleNeighbors = filter (/= c) $ (***) <$> transforms <*> transforms <*> [c]

numLightsOn :: Lights -> Int
numLightsOn = length . filter id . elems

-- Parser

parseLight :: Parser LightState
parseLight = (True <$ char '#') <|> (False <$ char '.')

listToArray :: [[LightState]] -> Lights
listToArray l = listArray ((0, 0), (length l - 1, length (head l) - 1)) $ concat l

parseInput :: Parser Lights
parseInput = listToArray <$> (many1 parseLight `sepEndBy1` newline <* eof)

part1 :: Lights -> IO ()
part1 = print . numLightsOn . repeatUpdate newLightState 100

part2 :: Lights -> IO ()
part2 = print . numLightsOn . repeatUpdate newLightStateWithBrokenCorners 100 . turnCornersOn
