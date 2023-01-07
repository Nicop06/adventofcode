module Day8(parseInput,part1
,part2) where
import Data.List (transpose, zipWith4)
import Text.Parsec
import Text.Parsec.String

-- Helpers

visibilityLine :: [Int] -> [Int]
visibilityLine l = (-1 :) . take (length l - 1) . scanl1 max $ l

visibilityGrid :: ([[Int]] -> [[Int]]) -> [[Int]] -> [[Int]]
visibilityGrid op = op . map visibilityLine . op

leftVisibility :: [[Int]] -> [[Int]]
leftVisibility = visibilityGrid id

rightVisibility :: [[Int]] -> [[Int]]
rightVisibility = visibilityGrid (map reverse)

topVisibility :: [[Int]] -> [[Int]]
topVisibility = visibilityGrid transpose

bottomVisibility :: [[Int]] -> [[Int]]
bottomVisibility = transpose . visibilityGrid (map reverse) . transpose

min4 :: Int -> Int -> Int -> Int -> Int
min4 a b c d = min (min a b) (min c d)

bestVisibility :: [[Int]] -> [Int]
bestVisibility l = zipWith4 min4 (concat $ leftVisibility l) (concat $ rightVisibility l) (concat $ topVisibility l) (concat $ bottomVisibility l)

numVisibleTrees :: [[Int]] -> Int
numVisibleTrees grid = length $ filter id $ zipWith (>) (concat grid) (bestVisibility grid)

getRow :: [[Int]] -> Int -> [Int]
getRow = (!!)

getCol :: [[Int]] -> Int -> [Int]
getCol l i = transpose l !! i

view :: [Int] -> Int -> Int
view l el =
  let numVisible = length (takeWhile (< el) l) + 1
   in min (length l) numVisible

scenicScore :: [[Int]] -> Int -> Int -> Int
scenicScore l row col =
  let (left, right) = splitAt col (getRow l row)
      (top, bottom) = splitAt row (getCol l col)
      el = head right
   in view (reverse left) el * view (tail right) el * view (reverse top) el * view (tail bottom) el

bestScenicScore :: [[Int]] -> Int
bestScenicScore l = maximum $ map (uncurry (scenicScore l)) indices
  where
    indices = [(i, j) | i <- [0 .. length l - 1], j <- [0 .. (length . head) l - 1]]

-- Parser

treeLine :: Parser [Int]
treeLine = many (read . (: []) <$> digit) <* newline

parseInput :: Parser [[Int]]
parseInput = many treeLine <* eof

part1 :: [[Int]] -> IO ()
part1 = print . numVisibleTrees

part2 :: [[Int]] -> IO ()
part2 = print . bestScenicScore
