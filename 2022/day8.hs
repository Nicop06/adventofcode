import Control.Monad
import Data.List
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

visibilityLine :: [Int] -> [Int]
visibilityLine l = (-1:) . take (length l - 1) . scanl1 max $ l

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

treeLine :: Parser [Int]
treeLine = many (read . (:[]) <$> digit) <* newline

treeGrid :: Parser [[Int]]
treeGrid = many treeLine <* eof

part1 :: Parser Int
part1 = numVisibleTrees <$> treeGrid

main :: IO ()
main = parseAndSolve "inputs/day8" part1 part1
