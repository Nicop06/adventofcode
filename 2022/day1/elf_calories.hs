import System.Environment
import Data.List

main = do
    (input:_) <- getArgs
    contents <- readFile input
    print . largestAmountCalories . groupInventories . lines $ contents

foldInventoryAux :: String -> [[Int]] -> [[Int]]
foldInventoryAux "" xs     = []:xs
foldInventoryAux el (x:xs) = (read el : x) : xs
foldInventoryAux _ []      = []

groupInventories :: [String] -> [[Int]]
groupInventories = foldr foldInventoryAux []

largestAmountCalories :: [[Int]] -> Int
largestAmountCalories = maximum . map sum
