module Day23
  ( part1,
    part2,
    parseInput,
  )
where

import Data.Foldable (maximumBy)
import Data.Function
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

inRange :: Nanobot -> Position -> Bool
inRange (Nanobot (x, y, z) r) (x', y', z') =
  abs (x' - x) + abs (y' - y) + abs (z' - z) <= r

norm :: Position -> Int
norm (x, y, z) = abs x + abs y + abs z

closestPointTo :: Position -> Nanobot -> Position
closestPointTo (tx, ty, tz) (Nanobot p@(x, y, z) r) =
  let d = norm p in (x + (tx - x) * r `div` d, y + (ty - y) * r `div` d, z + (tz - z) * r `div` d)

overlapsWithRegion :: Region -> Nanobot -> Bool
overlapsWithRegion ((x1, y1, z1), (x2, y2, z2)) b@(Nanobot (x, y, z) _) =
  (x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2)
    || inRange b (clamp (x1, x2) x, clamp (y1, y2) y, clamp (z1, z2) z)
  where
    clamp :: (Int, Int) -> Int -> Int
    clamp (_min, _max) n = max _min (min _max n)

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
    largestNanobotByRadius = maximumBy (compare `on` getRadius) nanobots

part2 :: [Nanobot] -> IO ()
part2 nanobots =
  print $
    length $
      filter (`inRange` (50000000, -50000000, 50000000)) nanobots
  where
    largestNanobotByRadius :: Nanobot
    largestNanobotByRadius = maximumBy (compare `on` getRadius) nanobots
