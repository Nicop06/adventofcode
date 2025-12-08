module Day18
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Set (Set, fromList, notMember, union)
import Text.Parsec
import Text.Parsec.String

-- Data

type Cube = (Int, Int, Int)

newtype Bounds = Bounds (Int, Int, Int) deriving (Show)

type Compound = [Cube]

-- Helpers

map1 :: (a -> a) -> (a, b, c) -> (a, b, c)
map1 f (a, b, c) = (f a, b, c)

map2 :: (b -> b) -> (a, b, c) -> (a, b, c)
map2 f (a, b, c) = (a, f b, c)

map3 :: (c -> c) -> (a, b, c) -> (a, b, c)
map3 f (a, b, c) = (a, b, f c)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, a, _) = a

thd3 :: (a, b, c) -> c
thd3 (_, _, a) = a

sidesCoordinates :: Cube -> [Cube]
sidesCoordinates cube = [map1, map2, map3] <*> [(+ 1), subtract 1] <*> [cube]

exposedSides :: Set Cube -> Cube -> [Cube]
exposedSides set = filter (`notMember` set) . sidesCoordinates

numExposedSides :: Set Cube -> Cube -> Int
numExposedSides set = length . exposedSides set

numExposedSidesWithoutPocket :: Set Cube -> Bounds -> Cube -> Int
numExposedSidesWithoutPocket set bounds =
  length . filter (canBeReachedByAir set bounds . fromList . (: [])) . exposedSides set

isOutsideCompound :: Bounds -> Cube -> Bool
isOutsideCompound (Bounds (mx, my, mz)) (x, y, z) =
  x > mx || y > my || z > mz || x < 0 || y < 0 || z < 0

compoundBounds :: Compound -> Bounds
compoundBounds c = Bounds (maximum . map fst3 $ c, maximum . map snd3 $ c, maximum . map thd3 $ c)

canBeReachedByAir :: Set Cube -> Bounds -> Set Cube -> Bool
canBeReachedByAir set bounds reachableCubes =
  case concatMap (exposedSides set) reachableCubes of
    [] -> False
    newReachable ->
      any (isOutsideCompound bounds) newReachable
        || canBeReachedByAir newSet bounds (fromList newReachable)
      where
        newSet = set `union` reachableCubes

totalExposedSidesWithoutPocket :: Compound -> Int
totalExposedSidesWithoutPocket c =
  let set = fromList c
      bounds = compoundBounds c
   in sum . map (numExposedSidesWithoutPocket set bounds) $ c

totalExposedSides :: Compound -> Int
totalExposedSides c = let set = fromList c in sum . map (numExposedSides set) $ c

-- Parser

parseCoordinate :: Parser Int
parseCoordinate = read <$> many1 digit <* optional (char ',')

parseCube :: Parser Cube
parseCube = (,,) <$> parseCoordinate <*> parseCoordinate <*> parseCoordinate <* newline

parseInput :: Parser Compound
parseInput = many1 parseCube <* eof

part1 :: Compound -> IO ()
part1 = print . totalExposedSides

part2 :: Compound -> IO ()
part2 = print . totalExposedSidesWithoutPocket
