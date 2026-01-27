module Day10
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.Array
import Data.Foldable (maximumBy)
import Data.List (elemIndex, sortBy, transpose)
import Data.Ord (comparing)
import Text.Parsec
import Text.Parsec.String

type Coord = (Int, Int)

type AsteroidMap = Array Coord Bool

toArray :: [[Bool]] -> AsteroidMap
toArray l =
  let w = length (head l)
      h = length l
      b = ((0, 0), (w - 1, h - 1))
   in listArray b (concat $ transpose l)

_showMap :: AsteroidMap -> [String]
_showMap m = [[toAscii $ m ! (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    ((minX, minY), (maxX, maxY)) = bounds m

    toAscii :: Bool -> Char
    toAscii True = '#'
    toAscii False = '.'

clockwiseOrdering :: Coord -> Coord -> Coord -> Ordering
clockwiseOrdering center c1 c2 =
  if quadrant1 /= quadrant2
    then comparing (`elemIndex` quadrantOrdering) quadrant1 quadrant2
    else compare (dy1 * dx2) (dy2 * dx1) -- decreasing order of x/y
  where
    (dx1, dy1) = dirVec center c1
    (dx2, dy2) = dirVec center c2
    quadrant1 = (signum dx1, signum dy1)
    quadrant2 = (signum dx2, signum dy2)
    quadrantOrdering =
      [ (0, -1),
        (1, -1),
        (1, 0),
        (1, 1),
        (0, 1),
        (-1, 1),
        (-1, 0),
        (-1, -1)
      ]

vaporisationOrder :: Coord -> AsteroidMap -> [Coord]
vaporisationOrder c = go
  where
    go :: AsteroidMap -> [Coord]
    go m =
      let visible = visibleAsteroids m c
       in if null visible
            then []
            else
              sortBy (clockwiseOrdering c) visible
                ++ go (m // [(v, False) | v <- visible])

-- Vector the first coordinate to the second coordinate,
-- normalised to the lowest integer.
dirVec :: Coord -> Coord -> Coord
dirVec (fx, fy) (tx, ty) =
  let (vx, vy) = (tx - fx, ty - fy)
      dt = gcd vx vy
   in (vx `div` dt, vy `div` dt)

isVisible :: AsteroidMap -> Coord -> Coord -> Bool
isVisible m (fx, fy) (tx, ty) =
  not $ any (m !) occluding
  where
    (vx, vy) = dirVec (fx, fy) (tx, ty)
    rangeX = [fx + vx, fx + 2 * vx .. tx - vx]
    rangeY = [fy + vy, fy + 2 * vy .. ty - vy]
    occluding
      | vx == 0 = (fx,) <$> rangeY
      | vy == 0 = (,fy) <$> rangeX
      | otherwise = zip rangeX rangeY

asteroidCoords :: AsteroidMap -> [Coord]
asteroidCoords = map fst . filter snd . assocs

visibleAsteroids :: AsteroidMap -> Coord -> [Coord]
visibleAsteroids m c = filter (isVisible m c) . filter (/= c) $ asteroidCoords m

maxAsteroidsVisible :: AsteroidMap -> (Coord, Int)
maxAsteroidsVisible m =
  maximumBy (comparing snd)
    . map (id &&& (length . visibleAsteroids m))
    $ asteroidCoords m

parseInput :: Parser AsteroidMap
parseInput =
  toArray
    <$> many1 ((True <$ char '#') <|> (False <$ char '.'))
    `sepEndBy1` newline
    <* eof

part1 :: AsteroidMap -> IO ()
part1 = print . snd . maxAsteroidsVisible

part2 :: AsteroidMap -> IO ()
part2 m = print (x * 100 + y)
  where
    laserPosition = fst $ maxAsteroidsVisible m
    (x, y) = vaporisationOrder laserPosition m !! 199
