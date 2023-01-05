import Data.List (groupBy, permutations, sortOn)
import Data.Map.Strict qualified as M
import Text.Parsec
import Text.Parsec.String

-- Data

data Reindeer = Reindeer { name :: String, speed :: Int, flyTime :: Int, restTime :: Int } deriving Show

-- Helpers

totalDistance :: Int -> Reindeer -> Int
totalDistance raceTime reindeer =
    let flyAndRestTime = flyTime reindeer + restTime reindeer
        distanceBetweenRest = flyTime reindeer * speed reindeer
        in
            distanceBetweenRest * (raceTime `div` flyAndRestTime) +
            speed reindeer * min (raceTime `mod` flyAndRestTime) (flyTime reindeer)

-- Parser

parseName :: Parser String
parseName = many1 alphaNum

parseSpeed :: Parser Int
parseSpeed = read <$> many1 digit <* string " km/s"

parseTime :: Parser Int
parseTime = read <$> many1 digit <* string " seconds"

parseReindeer :: Parser Reindeer
parseReindeer = Reindeer <$> parseName <*> (string " can fly " *> parseSpeed) <*> (string " for " *> parseTime) <*> (string ", but then must rest for " *> parseTime <* char '.')

parseInput :: IO (Either ParseError [Reindeer])
parseInput = parseFromFile (parseReindeer `sepEndBy1` newline <* eof) "inputs/day14"

part1 :: [Reindeer] -> IO ()
part1 = print . maximum . map (totalDistance 2503)

part2 :: String -> IO ()
part2 = print

main :: IO ()
main = parseInput >>= either print part1
