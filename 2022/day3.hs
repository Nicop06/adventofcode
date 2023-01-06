import Data.Char
import Data.List (intersect)
import ParseAndRun

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

part2 :: [String] -> Int
part2 (elf1 : elf2 : elf3 : rest) = itemPriority groupItem + part2 rest
  where
    groupItem = head (elf1 `intersect` elf2 `intersect` elf3)
part2 _ = 0

main :: IO ()
main = parseAndRun "inputs/day3" part1 part2
