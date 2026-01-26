module Day8
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Text.Parsec
import Text.Parsec.String

width :: Int
width = 25

height :: Int
height = 6

type Layer = [Int]

type Image = [Layer]

parseImage :: [Int] -> Image
parseImage [] = []
parseImage d =
  let (l, ls) = splitAt (width * height) d
   in l : parseImage ls

decodeImage :: Image -> [[Int]]
decodeImage = reshape . mergeLayers
  where
    reshape :: [Int] -> [[Int]]
    reshape [] = []
    reshape d =
      let (l, ls) = splitAt width d
       in l : reshape ls

    mergeLayers :: [[Int]] -> [Int]
    mergeLayers [] = []
    mergeLayers ([] : _) = []
    mergeLayers l = head (dropWhile (== 2) (map head l)) : mergeLayers (map tail l)

parseInput :: Parser Image
parseInput =
  parseImage
    <$> many1 (read . pure <$> digit)
    <* newline
    <* eof

part1 :: Image -> IO ()
part1 image = print result
  where
    layer = minimumBy (comparing (numElems 0)) image
    result = numElems 1 layer * numElems 2 layer

    numElems :: Int -> Layer -> Int
    numElems d = length . filter (== d)

part2 :: Image -> IO ()
part2 = mapM_ (putStrLn . map toPixel) . decodeImage
  where
    toPixel :: Int -> Char
    toPixel 0 = ' ' -- Black
    toPixel 1 = '█' -- White
    toPixel i = error $ "Unknown pixel " ++ show i
