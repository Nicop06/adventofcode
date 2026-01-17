{-# LANGUAGE RankNTypes #-}

module Day17
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Tuple (swap)
import Text.Parsec hiding (State)
import Text.Parsec.String

data WaterType = Still | Falling deriving (Show)

data Tile = Clay | Water WaterType | Sand deriving (Show)

type GroundMap = Array Coord Tile

type GroundMapST s = STArray s Coord Tile

type Coord = (Int, Int)

waterSpring :: Coord
waterSpring = (500, 0)

isWater :: Tile -> Bool
isWater (Water _) = True
isWater _ = False

isStillWater :: Tile -> Bool
isStillWater (Water Still) = True
isStillWater _ = False

spread :: [Coord] -> GroundMapST s -> ST s (Maybe Coord, [Coord])
spread [] _ = return (Nothing, [])
spread (c@(x, y) : cs) a = do
  t <- readArray a c
  case t of
    Clay -> return (Nothing, [])
    _ -> do
      t' <- readArray a (x, y + 1)
      case t' of
        Clay -> fmap (second (c :)) (spread cs a)
        Water Still -> fmap (second (c :)) (spread cs a)
        _ -> return (Just c, [c])

runSpread :: Coord -> GroundMapST s -> ST s [Coord]
runSpread (startX, startY) a = do
  l <- spread (map (,startY) [startX, startX - 1 ..]) a
  r <- spread (map (,startY) [startX ..]) a
  let fallingCoord = mapMaybe fst [l, r]
      waterType = if null fallingCoord then Still else Falling
   in do
        forM_ (snd r ++ snd l) (\c -> writeArray a c (Water waterType))
        return fallingCoord

flowDown :: GroundMapST s -> Coord -> ST s [Coord]
flowDown a (x, y) = do
  b <- getBounds a
  if not (inRange b (x, y))
    then return []
    else do
      t <- readArray a (x, y)
      case t of
        Clay -> runSpread (x, y - 1) a
        Water Still -> runSpread (x, y - 1) a
        _ -> do
          writeArray a (x, y) (Water Falling)
          flowDown a (x, y + 1)

runWater :: [Coord] -> [(Coord, Tile)]
runWater coords =
  let minX = minimum (map fst coords)
      minY = minimum (map snd coords)
      maxX = maximum (map fst coords)
      maxY = maximum (map snd coords)
      b = ((minX - 1, 0), (maxX + 1, maxY))
      initMap = array b [(c, Sand) | c <- range b] // map (,Clay) coords
   in filter ((>= minY) . snd . fst)
        . assocs
        $ runSTArray (thaw initMap >>= go (S.singleton waterSpring))
  where
    go :: S.Set Coord -> GroundMapST s -> ST s (GroundMapST s)
    go cs a = do
      numWater <- foldlMArray' (\n t -> n + fromEnum (isWater t)) 0 a
      cs' <- fmap (S.fromList . concat) (mapM (flowDown a) (S.elems cs))
      numWater' <- foldlMArray' (\n t -> n + fromEnum (isWater t)) 0 a
      if numWater' <= numWater
        then return a
        else go (S.union cs cs') a

_toAscii :: GroundMap -> [String]
_toAscii a = [[drawTile (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    ((minX, minY), (maxX, maxY)) = bounds a

    drawTile :: Coord -> Char
    drawTile c = case a ! c of
      Clay -> '#'
      Water Still -> '~'
      Water Falling -> '|'
      Sand -> '.'

parseInput :: Parser [Coord]
parseInput = concat <$> parseRow `sepEndBy1` newline <* eof
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 digit

    parseRange :: Parser [Int]
    parseRange = do
      minVal <- parseInt
      _ <- string ".."
      maxVal <- parseInt
      return [minVal .. maxVal]

    parseVal :: Parser [Int]
    parseVal = try parseRange <|> (pure <$> parseInt)

    parseCoords :: Char -> Char -> Parser [Coord]
    parseCoords c1 c2 = do
      coords1 <- between (string (c1 : "=")) (string (", " ++ [c2] ++ "=")) parseVal
      coords2 <- parseVal
      return $ (,) <$> coords1 <*> coords2

    parseRow :: Parser [Coord]
    parseRow = parseCoords 'x' 'y' <|> (map swap <$> parseCoords 'y' 'x')

part1 :: [Coord] -> IO ()
part1 =
  print
    . length
    . filter (isWater . snd)
    . runWater

part2 :: [Coord] -> IO ()
part2 =
  print
    . length
    . filter (isStillWater . snd)
    . runWater
