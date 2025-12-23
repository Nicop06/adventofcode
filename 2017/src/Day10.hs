module Day10
  ( part1,
    part2,
  )
where

import Control.Monad ((>=>))
import Data.Bits (xor)
import Data.Char (ord)
import Text.Parsec
import Text.Parsec.String
import Text.Printf (printf)

data CircularList = CircularList Int [Int] deriving (Show, Eq)

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

parsePart1 :: Parser [Int]
parsePart1 = (read <$> many1 digit) `sepEndBy1` char ',' <* newline <* eof

solvePart1 :: [Int] -> Int
solvePart1 = product . take 2 . simpleKnotHash [0 .. 255]

part1 :: FilePath -> IO ()
part1 file = parseFromFile parsePart1 file >>= either print (print . solvePart1)

------------
-- Part 2 --
------------

chunksOf :: Int -> [Int] -> [[Int]]
chunksOf _ [] = []
chunksOf n l = let (chunk, rs) = splitAt n l in chunk : chunksOf n rs

toDenseHash :: [Int] -> [Int]
toDenseHash = map (foldr1 xor) . chunksOf 16

parseInput :: String -> [Int]
parseInput =
  concat
    . replicate 64
    . (++ [17, 31, 73, 47, 23])
    . map ord
    . filter (/= '\n')

toHex :: [Int] -> String
toHex = concatMap (printf "%02x")

knotHash :: String -> String
knotHash =
  toHex
    . toDenseHash
    . simpleKnotHash [0 .. 255]
    . parseInput

part2 :: FilePath -> IO ()
part2 = readFile >=> putStrLn . knotHash
