module Day14
  ( part1,
    part2,
  )
where

import Control.Arrow (first, second)
import Control.Monad ((>=>))
import Data.Array ((!))
import qualified Data.Array as A
import Data.Bits (xor)
import Data.Char (ord)
import qualified Data.Set as S

data CircularList = CircularList Int [Int] deriving (Show, Eq)

type Coord = (Int, Int)

type Grid = A.Array Coord Bool

twist :: Int -> CircularList -> CircularList
twist len (CircularList pos l) =
  let (start, end) = splitAt (len `mod` length l) l
   in CircularList (pos + len) (end ++ reverse start)

skip :: Int -> CircularList -> CircularList
skip n (CircularList pos l) =
  let (start, end) = splitAt (n `mod` length l) l
   in CircularList (pos + n) (end ++ start)

toList :: CircularList -> [Int]
toList (CircularList pos l) =
  let (start, end) = splitAt (len - (pos `mod` len)) l
   in end ++ start
  where
    len = length l

fromList :: [Int] -> CircularList
fromList = CircularList 0

simpleKnotHash :: [Int] -> [Int] -> [Int]
simpleKnotHash els = toList . snd . foldl go (0, fromList els)
  where
    go :: (Int, CircularList) -> Int -> (Int, CircularList)
    go (s, l) len = (s + 1, skip s $ twist len l)

chunksOf :: Int -> [Int] -> [[Int]]
chunksOf _ [] = []
chunksOf n l = let (chunk, rs) = splitAt n l in chunk : chunksOf n rs

toDenseHash :: [Int] -> [Int]
toDenseHash = map (foldr1 xor) . chunksOf 16

makeHashInput :: String -> [Int]
makeHashInput =
  concat
    . replicate 64
    . (++ [17, 31, 73, 47, 23])
    . map ord
    . filter (/= '\n')

knotHash :: String -> [Int]
knotHash =
  toDenseHash
    . simpleKnotHash [0 .. 255]
    . makeHashInput

toBits :: Int -> Int -> [Bool]
toBits b i
  | b == 0 = []
  | otherwise = toBits (b `div` 2) (i `div` 2) ++ [toEnum (i `mod` 2)]

makeRow :: String -> Int -> [Bool]
makeRow s r = concatMap (toBits 128) $ knotHash (s ++ "-" ++ show r)

makeGrid :: String -> [[Bool]]
makeGrid s = map (makeRow s) [0 .. 127]

usedSquares :: String -> Int
usedSquares = sum . map numBits . makeGrid
  where
    numBits :: [Bool] -> Int
    numBits = sum . map fromEnum

getNeighbours :: Coord -> [Coord]
getNeighbours c = [first, second] <*> [(+ 1), subtract 1] <*> [c]

numGroups :: [[Bool]] -> Int
numGroups el = numConnectedComponents (S.fromList usedCoords)
  where
    b :: (Coord, Coord)
    b = ((0, 0), (127, 127))

    grid :: Grid
    grid = A.listArray b (concat el)

    usedCoords :: [Coord]
    usedCoords = map fst . filter snd $ A.assocs grid

    connectedComponent :: [Coord] -> S.Set Coord -> S.Set Coord
    connectedComponent [] s = s
    connectedComponent (c : cs) s
      | c `S.member` s = connectedComponent cs s
      | otherwise = connectedComponent (cs ++ neighbours) (S.insert c s)
      where
        neighbours =
          filter (grid !) . filter (A.inRange b) $
            getNeighbours c

    numConnectedComponents :: S.Set Coord -> Int
    numConnectedComponents s = case S.elems s of
      [] -> 0
      (c : _) ->
        let comp = connectedComponent [c] S.empty
         in 1 + numConnectedComponents (s `S.difference` comp)

part1 :: FilePath -> IO ()
part1 = readFile >=> print . usedSquares

part2 :: FilePath -> IO ()
part2 = readFile >=> print . numGroups . makeGrid
