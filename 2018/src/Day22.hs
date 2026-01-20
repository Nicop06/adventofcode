module Day22
  ( part1,
    part2,
    parseInput,
  )
where

import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array
import Data.Array.ST
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec hiding (State)
import Text.Parsec.String

type Coord = (Int, Int)

data Type = Rocky | Wet | Narrow deriving (Enum, Show)

data Tool = Torch | Climbing | Neither deriving (Eq, Ord, Show)

data Puzzle = Puzzle Int Coord deriving (Show)

type DistanceState a = State (S.Set (Int, Coord, Tool), M.Map (Coord, Tool) Int) a

regionType :: Int -> Type
regionType = toEnum . (`mod` 3)

tools :: Type -> [Tool]
tools Rocky = [Climbing, Torch]
tools Wet = [Climbing, Neither]
tools Narrow = [Torch, Neither]

buildMap :: Int -> Puzzle -> Array Coord Type
buildMap buffer (Puzzle depth (tx, ty)) =
  fmap regionType $
    runSTArray $ do
      ar <- newArray rect Nothing
      newGenArray rect (erosionLevel ar)
  where
    rect = ((0, 0), (tx + buffer, ty + buffer))

    erosionLevel :: STArray s Coord (Maybe Int) -> (Int, Int) -> ST s Int
    erosionLevel ar (x, y) = do
      val <- readArray ar (x, y)
      case val of
        Just idx -> return idx
        Nothing -> do
          idx <- (`mod` 20183) . (+ depth) <$> geologicIndex ar (x, y)
          writeArray ar (x, y) (Just idx)
          return idx

    geologicIndex :: STArray s Coord (Maybe Int) -> (Int, Int) -> ST s Int
    geologicIndex ar (x, y)
      | (x, y) == (0, 0) = return 0
      | (x, y) == (tx, ty) = return 0
      | x == 0 = return (y * 48271)
      | y == 0 = return (x * 16807)
      | otherwise = do
          left <- erosionLevel ar (x - 1, y)
          top <- erosionLevel ar (x, y - 1)
          return (left * top)

distanceToTarget :: Coord -> Array Coord Type -> Int
distanceToTarget target areaMap =
  evalState (go (0, (0, 0), Torch)) (S.empty, M.empty)
  where
    b = bounds areaMap

    toolsAvailable :: Coord -> [Tool]
    toolsAvailable c = tools (areaMap ! c)

    canAccess :: Coord -> Tool -> Bool
    canAccess c g = g `elem` toolsAvailable c

    neighbours :: Coord -> [(Coord, Tool)]
    neighbours (x, y) =
      filter (uncurry canAccess) $
        (,)
          <$> filter
            (inRange b)
            [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
          <*> toolsAvailable (x, y)

    go :: (Int, Coord, Tool) -> DistanceState Int
    go (d, c, g)
      | c == target && g == Torch = return d
      | otherwise = do
          modify $ first (S.delete (d, c, g))
          updateDistance (c, g, d)
          (s, _) <- get
          go $ S.findMin s

    updateDistance :: (Coord, Tool, Int) -> DistanceState ()
    updateDistance (c, g, d) = do
      forM_ (neighbours c) $ \(c', g') -> do
        (_, m) <- get
        let d' = d + if g == g' then 1 else 8
        let oldD = M.findWithDefault maxBound (c', g') m
        when (d' < oldD) $ do
          modify $ first (S.delete (oldD, c', g'))
          modify $ first (S.insert (d', c', g'))
          modify $ second (M.insert (c', g') d')

parseInput :: Parser Puzzle
parseInput =
  Puzzle
    <$> between (string "depth: ") newline parseInt
    <*> between (string "target: ") newline parseCoord
    <* eof
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 digit

    parseCoord :: Parser Coord
    parseCoord = (,) <$> (parseInt <* char ',') <*> parseInt

part1 :: Puzzle -> IO ()
part1 = print . sum . fmap fromEnum . buildMap 0

part2 :: Puzzle -> IO ()
part2 p@(Puzzle _ target) = print . distanceToTarget target $ buildMap 2000 p
