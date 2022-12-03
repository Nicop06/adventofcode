import Data.Char
import Data.List
import System.Environment

main :: IO ()
main = do
  (input : _) <- getArgs
  contents <- readFile input
  print $ totalPriority (lines contents)

itemPriority :: Char -> Int
itemPriority item
  | isAsciiLower item = ord item - ord 'a' + 1
  | isAsciiUpper item = ord item - ord 'A' + 27
  | otherwise = 0

duplicateItem :: String -> Char
duplicateItem items = head $ c1 `intersect` c2
  where
    (c1, c2) = splitAt (length items `div` 2) items

totalPriority :: [String] -> Int
totalPriority = sum . map (itemPriority . duplicateItem)
