import ParseAndRun
import Data.Char
import Data.List

itemPriority :: Char -> Int
itemPriority item
  | isAsciiLower item = ord item - ord 'a' + 1
  | isAsciiUpper item = ord item - ord 'A' + 27
  | otherwise = 0

duplicateItem :: String -> Char
duplicateItem items = head $ c1 `intersect` c2
  where
    (c1, c2) = splitAt (length items `div` 2) items

part1 :: [String] -> Int
part1 = sum . map (itemPriority . duplicateItem)

main :: IO ()
main = parseAndRun "inputs/day3" part1 part1
