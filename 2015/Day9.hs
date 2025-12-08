module Day9
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (groupBy, permutations, sortOn)
import Data.Map.Strict as M (Map, fromList, keys, lookup)
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

-- Data

type City = String

type Distance = Int

type CityDistance = Map City [(City, Distance)]

-- Helpers

buildCityDistanceMap :: [(City, (City, Distance))] -> CityDistance
buildCityDistanceMap = fromList . map cityPair . groupBy sameCity . sortOn fst . concatMap reversedDistance
  where
    reversedDistance p@(c1, (c2, d)) = [p, (c2, (c1, d))]
    sameCity (c1, _) (c2, _) = c1 == c2
    cityPair cities = (fst . head $ cities, map snd cities)

pathLength :: CityDistance -> [City] -> Int
pathLength distances = sum . map pairDistance . pathPairs
  where
    pairDistance (c1, c2) =
      let distToCity = fromJust $ M.lookup c1 distances
       in snd . head . filter ((== c2) . fst) $ distToCity

pathPairs :: [City] -> [(City, City)]
pathPairs [] = []
pathPairs [_] = []
pathPairs (a : b : rs) = (a, b) : pathPairs (b : rs)

allPathLength :: CityDistance -> [Int]
allPathLength distances = map (pathLength distances) . permutations $ M.keys distances

-- Parser

parseCity :: Parser City
parseCity = many1 alphaNum

parseDistance :: Parser Distance
parseDistance = read <$> many1 digit

parseInput :: Parser CityDistance
parseInput = buildCityDistanceMap <$> (parseCityDistancePair `sepEndBy1` newline) <* eof
  where
    parseCityDistancePair = (,) <$> parseCity <*> parseCityDistance
    parseCityDistance = (,) <$> (string " to " *> parseCity) <*> (string " = " *> parseDistance)

part1 :: CityDistance -> IO ()
part1 = print . minimum . allPathLength

part2 :: CityDistance -> IO ()
part2 = print . maximum . allPathLength
