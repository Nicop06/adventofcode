import Data.List
import ParseAndRun
import System.Environment

foldInventoryAux :: String -> [[Int]] -> [[Int]]
foldInventoryAux "" xs = [] : xs
foldInventoryAux el (x : xs) = (read el : x) : xs
foldInventoryAux _ [] = []

groupInventories :: [String] -> [[Int]]
groupInventories = foldr foldInventoryAux []

largestAmountCaloriesTopK :: Int -> [[Int]] -> Int
largestAmountCaloriesTopK k = sum . take k . reverse . sort . map sum

solution :: Int -> [String] -> Int
solution k = largestAmountCaloriesTopK k . groupInventories

main :: IO ()
main = parseAndRun "inputs/day1" (solution 1) (solution 3)
