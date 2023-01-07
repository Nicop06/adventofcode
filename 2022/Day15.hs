module Day15(parseInput,part1
,part2) where
import Data.Bifunctor (bimap)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type Position = (Int, Int)

type Sensor = Position

type Beacon = Position

type Interval = (Int, Int)

type BeaconDetection = (Sensor, Beacon)

-- Helpers

manhattanDistance :: Sensor -> Beacon -> Int
manhattanDistance (xs, ys) (xb, yb) = abs (xb - xs) + abs (yb - ys)

coveredIntervalsInRow :: Int -> [BeaconDetection] -> [Interval]
coveredIntervalsInRow row = mapMaybe intervalCovered
  where
    intervalCovered (s, b) =
      let distToRow = abs (snd s - row)
          distToBeacon = manhattanDistance s b
          distDiff = distToBeacon - distToRow
       in if distDiff < 0 then Nothing else Just (fst s - distDiff, fst s + distDiff)

mergeIntervals :: [Interval] -> [Interval]
mergeIntervals [] = []
mergeIntervals [i] = [i]
mergeIntervals ((min1, max1) : (min2, max2) : rs)
  | min2 <= max1 + 1 = mergeIntervals ((min1, max max1 max2) : rs)
  | otherwise = (min1, max1) : mergeIntervals ((min2, max2) : rs)

mergedCoveredIntervals :: Int -> [BeaconDetection] -> [Interval]
mergedCoveredIntervals row = mergeIntervals . sort . coveredIntervalsInRow row

intervalSize :: Interval -> Int
intervalSize (from, to) = to - from + 1

numIntervalCovered :: Int -> [BeaconDetection] -> Int
numIntervalCovered row = sum . map intervalSize . mergedCoveredIntervals row

numPositionsWithoutBeacon :: Int -> [BeaconDetection] -> Int
numPositionsWithoutBeacon row = (-) <$> numIntervalCovered row <*> numBeaconsOnRow
  where
    numBeaconsOnRow = length . nub . filter (== row) . map (snd . snd)

clampInterval :: Int -> Int -> Interval -> Interval
clampInterval from to = bimap (max from) (min to)

distressBeaconRow :: Int -> Int -> [BeaconDetection] -> (Int, [Interval])
distressBeaconRow from to l = head $ filter ((> 1) . length . snd) intervalsForAllRows
  where
    mergeAndClamp row = (row, map (clampInterval from to) $ mergedCoveredIntervals row l)
    intervalsForAllRows = map mergeAndClamp [from .. to]

tuningFrequency :: Int -> [Interval] -> Int
tuningFrequency row int = row + 4000000 * (snd (head int) + 1)

-- Parser

parsePosition :: Parser Position
parsePosition = (,) <$> (parseCoord 'x' <* string ", ") <*> parseCoord 'y'

parseCoord :: Char -> Parser Int
parseCoord c = read <$> (char c *> string "=" *> many1 (digit <|> char '-'))

parseSensor :: Parser Sensor
parseSensor = string "Sensor at " *> parsePosition

parseBeacon :: Parser Beacon
parseBeacon = string ": closest beacon is at " *> parsePosition

parseBeaconDetection :: Parser BeaconDetection
parseBeaconDetection = (,) <$> parseSensor <*> parseBeacon <* newline

parseInput :: Parser [BeaconDetection]
parseInput = many1 parseBeaconDetection <* eof

rowToReport :: Int
rowToReport = 2000000

part1 :: [BeaconDetection] -> IO ()
part1 = print . numPositionsWithoutBeacon rowToReport

part2 :: [BeaconDetection] -> IO ()
part2 = print . uncurry tuningFrequency . distressBeaconRow 0 4000000
