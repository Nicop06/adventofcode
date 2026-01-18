module Day18
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.Array
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

data Tile = OpenGround | Tree | Lumberyard deriving (Eq, Show, Ord)

type Coord = (Int, Int)

type AreaMap = Array Coord Tile

neighbours :: Coord -> [Coord]
neighbours c = filter (/= c) $ (***) <$> transforms <*> transforms <*> [c]
  where
    transforms = [(+ 1), id, subtract 1]

nextTile :: AreaMap -> Coord -> Tile
nextTile area c =
  let adjacentTiles = map (area !) $ filter (inRange (bounds area)) (neighbours c)
      numTrees = length $ filter (Tree ==) adjacentTiles
      numLumbyards = length $ filter (Lumberyard ==) adjacentTiles
   in case area ! c of
        OpenGround -> if numTrees >= 3 then Tree else OpenGround
        Tree -> if numLumbyards >= 3 then Lumberyard else Tree
        Lumberyard -> if numTrees >= 1 && numLumbyards >= 1 then Lumberyard else OpenGround

changeArea :: AreaMap -> AreaMap
changeArea area = let b = bounds area in array b [(c, nextTile area c) | c <- range b]

evolve :: Int -> AreaMap -> AreaMap
evolve n = go 0 M.empty
  where
    go :: Int -> M.Map AreaMap Int -> AreaMap -> AreaMap
    go i cache area
      | i == n = area
      | otherwise = case M.lookup area cache of
          Nothing -> go (i + 1) (M.insert area i cache) (changeArea area)
          Just j ->
            let period = i - j
                numPeriods = (n - j) `div` period
             in go (j + numPeriods * period) M.empty area

totalResources :: AreaMap -> Int
totalResources area =
  let numTrees = length $ filter (Tree ==) $ elems area
      numLumbyards = length $ filter (Lumberyard ==) $ elems area
   in numLumbyards * numTrees

parseInput :: Parser AreaMap
parseInput = toArray <$> many1 parseTile `sepEndBy1` newline <* eof
  where
    parseTile :: Parser Tile
    parseTile =
      (OpenGround <$ char '.')
        <|> (Tree <$ char '|')
        <|> (Lumberyard <$ char '#')

    toArray :: [[Tile]] -> AreaMap
    toArray l =
      let h = length l
          w = length (head l)
          b = ((1, 1), (h, w))
       in listArray b (concat l)

_toAscii :: AreaMap -> [String]
_toAscii area =
  let ((minX, minY), (maxX, maxY)) = bounds area
   in [[tileToChar (area ! (x, y)) | y <- range (minY, maxY)] | x <- range (minX, maxX)]
  where
    tileToChar :: Tile -> Char
    tileToChar OpenGround = '.'
    tileToChar Tree = '|'
    tileToChar Lumberyard = '#'

part1 :: AreaMap -> IO ()
part1 = print . totalResources . evolve 10

part2 :: AreaMap -> IO ()
part2 = print . totalResources . evolve 1000000000
