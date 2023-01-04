import Data.List (groupBy, sortOn)
import Data.Map.Strict qualified as M
import Text.Parsec
import Text.Parsec.String

-- Data

type City = String

type Distance = Int

type CityDistance = M.Map City [(City, Distance)]

-- Helpers

buildCityDistanceMap :: [(City, (City, Distance))] -> CityDistance
buildCityDistanceMap = M.fromList . map cityPair . groupBy sameCity . sortOn fst
  where
    reversedDistance (c1, (c2, d)) = (c2, (c1, d))
    sameCity (c1, _) (c2, _) = c1 == c2
    cityPair cities = (fst . head $ cities, map snd cities)

allPaths :: CityDistance -> [(City, Distance)]
allPaths distances = map remainingPaths (M.keys distances)
    where
        remainingPaths city = case M.lookup city distances of
            Nothing -> ("a", 2)
            Just c -> head c

-- Parser

parseCity :: Parser City
parseCity = many1 alphaNum

parseDistance :: Parser Distance
parseDistance = read <$> many1 digit

parseCityDistanceMap :: Parser CityDistance
parseCityDistanceMap = buildCityDistanceMap <$> (parseCityDistancePair `sepEndBy1` newline)
  where
    parseCityDistancePair = (,) <$> parseCity <*> parseCityDistance
    parseCityDistance = (,) <$> (string " to " *> parseCity) <*> (string " = " *> parseDistance)

parseInput :: IO (Either ParseError CityDistance)
parseInput = parseFromFile (parseCityDistanceMap <* eof) "inputs/day9"

main :: IO ()
main = parseInput >>= either print print
