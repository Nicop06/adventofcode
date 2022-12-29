import Data.List (group, isInfixOf)
import Text.Parsec
import Text.Parsec.String

-- Part 1

hasThreeVowels :: String -> Bool
hasThreeVowels = (>= 3) . length . filter (`elem` "aeiou")

hasLetterAppearingTwice :: String -> Bool
hasLetterAppearingTwice = (> 1) . maximum . map length . group

noNaughtyString :: String -> Bool
noNaughtyString str = not $ any (`isInfixOf` str) ["ab", "cd", "pq", "xy"]

isNiceString :: String -> Bool
isNiceString str = hasThreeVowels str && hasLetterAppearingTwice str && noNaughtyString str

part1 :: [String] -> IO ()
part1 = print . length . filter isNiceString

-- Part 2

containsPairLettersTwice :: String -> Bool
containsPairLettersTwice [] = False
containsPairLettersTwice str = uncurry isInfixOf (splitAt 2 str) || containsPairLettersTwice (tail str)

repeatWithLetterBetween :: String -> Bool
repeatWithLetterBetween str
  | length str > 2 = (head str == str !! 2) || repeatWithLetterBetween (tail str)
  | otherwise = False

isNiceStringRevised :: String -> Bool
isNiceStringRevised str = containsPairLettersTwice str && repeatWithLetterBetween str

part2 :: [String] -> IO ()
part2 = print . length . filter isNiceStringRevised

-- Parse

parseInput :: IO (Either ParseError [String])
parseInput = parseFromFile (many1 lower `sepEndBy1` newline <* eof) "inputs/day5"

main = parseInput >>= either print part2
