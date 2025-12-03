module Day2
  ( parseInput
  , part1
  , part2
  , isInvalidId
  ) where

import Text.Parsec
import Text.Parsec.String

data Range =
  Range Int Int
  deriving (Show, Eq, Read)

allCuts :: String -> [String]
allCuts s = map (\x -> drop x s) $ [0 .. (length s - 1)]

repeatString :: Int -> String -> String
repeatString n = concat . take n . repeat

isInvalidId :: (Int -> [Int]) -> Int -> Bool
isInvalidId numRepeats i =
  not $ null $ filter (== s) $ repeatString <$> numRepeats i <*> allCuts s
  where
    s = show i

invalidIds :: (Int -> [Int]) -> Range -> [Int]
invalidIds numRepeats (Range from to)
  | from > to = invalidIds numRepeats (Range to from)
  | otherwise = filter (isInvalidId numRepeats) [from .. to]

parseRange :: Parser Range
parseRange =
  Range . read <$> many1 digit <* string "-" <*> (read <$> many1 digit)

parseInput :: Parser [Range]
parseInput = parseRange `sepBy1` (string ",") <* newline <* eof

part1 :: [Range] -> IO ()
part1 = print . foldr (+) 0 . concat . map (invalidIds (const [2]))

part2 :: [Range] -> IO ()
part2 = print . foldr (+) 0 . concat . map (invalidIds allLengths)
  where
    allLengths i = [2 .. (length (show i))]
