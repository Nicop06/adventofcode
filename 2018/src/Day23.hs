module Day23
  ( part1,
    part2,
    parseInput,
  )
where

import Control.Arrow
import Data.Foldable (maximumBy)
import Data.Ix (range)
import Data.Ord (comparing)
import Text.Parsec hiding (getPosition)
import Text.Parsec.String

type Position = (Int, Int, Int)

type Radius = Int

type Region = (Position, Position)

data Nanobot = Nanobot
  { getPosition :: Position,
    getRadius :: Radius
  }
  deriving (Show)

distance :: Position -> Position -> Int
distance (x, y, z) (x', y', z') = abs (x' - x) + abs (y' - y) + abs (z' - z)

inRange :: Nanobot -> Position -> Bool
inRange (Nanobot p r) p' = distance p p' <= r

inRangeOfRegion :: Region -> Nanobot -> Bool
inRangeOfRegion ((x1, y1, z1), (x2, y2, z2)) b@(Nanobot (x, y, z) _) =
  (x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2)
    || inRange b (clamp (x1, x2) x, clamp (y1, y2) y, clamp (z1, z2) z)
  where
    clamp :: (Int, Int) -> Int -> Int
    clamp (_min, _max) n = max _min (min _max n)

combine :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Region
combine (x1, x2) (y1, y2) (z1, z2) = ((x1, y1, z1), (x2, y2, z2))

regionSize :: Region -> Int
regionSize ((x1, y1, z1), (x2, y2, z2)) = maximum [x2 - x1, y2 - y1, z2 - z1]

divideRegion :: Region -> [Region]
divideRegion ((x1, y1, z1), (x2, y2, z2)) =
  let midx = x1 + (x2 - x1) `div` 2
      midy = y1 + (y2 - y1) `div` 2
      midz = z1 + (z2 - z1) `div` 2
   in combine
        <$> [(x1, midx), (midx + 1, x2)]
        <*> [(y1, midy), (midy + 1, y2)]
        <*> [(z1, midz), (midz + 1, z2)]

bestLocation :: [Nanobot] -> Position
bestLocation bots = go [initRegion]
  where
    maxCoord :: Position -> Int
    maxCoord (x, y, z) = maximum [abs x, abs y, abs z]

    initRegion =
      let size = maximum (map (maxCoord . getPosition) bots)
       in ((-size, -size, -size), (size, size, size))

    numInRangeRegion :: Region -> Int
    numInRangeRegion r = length $ filter (inRangeOfRegion r) bots

    bestRegions :: [Region] -> [Region]
    bestRegions rs = map fst $ filter ((== bestNum) . snd) regionAndNum
      where
        regionAndNum = map (id &&& numInRangeRegion) rs
        bestNum = maximum (map snd regionAndNum)

    bestPosition :: [Position] -> Position
    bestPosition = maximumBy (comparing numInRange)

    numInRange :: Position -> Int
    numInRange p = length $ filter (`inRange` p) bots

    go :: [Region] -> Position
    go rs
      | any ((> 10) . regionSize) rs = go (bestRegions $ concatMap divideRegion rs)
      | otherwise = bestPosition (concatMap range rs)

parseInput :: Parser [Nanobot]
parseInput = parseNanobot `sepEndBy1` newline <* eof
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 (digit <|> char '-')

    parsePosition :: Parser Position
    parsePosition =
      between
        (string "pos=<")
        (string ">, r=")
        ( (,,)
            <$> (parseInt <* char ',')
            <*> (parseInt <* char ',')
            <*> parseInt
        )

    parseNanobot :: Parser Nanobot
    parseNanobot = Nanobot <$> parsePosition <*> parseInt

part1 :: [Nanobot] -> IO ()
part1 nanobots =
  print $
    length $
      filter (inRange largestNanobotByRadius . getPosition) nanobots
  where
    largestNanobotByRadius :: Nanobot
    largestNanobotByRadius = maximumBy (comparing getRadius) nanobots

part2 :: [Nanobot] -> IO ()
part2 = print . distance (0, 0, 0) . bestLocation
