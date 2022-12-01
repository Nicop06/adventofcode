import Data.List
import System.Environment

main = do
  (input : k : _) <- getArgs
  contents <- readFile input
  print . largestAmountCaloriesTopK (read k) . groupInventories . lines $ contents

foldInventoryAux :: String -> [[Int]] -> [[Int]]
foldInventoryAux "" xs = [] : xs
foldInventoryAux el (x : xs) = (read el : x) : xs
foldInventoryAux _ [] = []

groupInventories :: [String] -> [[Int]]
groupInventories = foldr foldInventoryAux []

largestAmountCaloriesTopK :: Int -> [[Int]] -> Int
largestAmountCaloriesTopK k = sum . take k . reverse . sort . map sum
