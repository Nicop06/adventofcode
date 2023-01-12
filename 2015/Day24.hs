module Day24
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

-- Data

type Weight = Int

type Group = [Weight]

-- Helpers

allPossibleGroupsWithNumElem :: Int -> [Weight] -> [Group]
allPossibleGroupsWithNumElem n weights =
  filter ((== totalWeight) . (* n) . sum) $ subsets weights
  where
    totalWeight = sum weights
    subsets [] = [[]]
    subsets (w : ws) =
      let s = subsets ws
       in filter ((<= totalWeight) . (* n) . sum) (s ++ map (w :) s)

allValidGroups :: [Weight] -> [Group]
allValidGroups weights = filter groupIsValid $ filter ((<= maxLen) . length) groups
  where
    maxLen = length weights `div` 3
    groups = allPossibleGroupsWithNumElem 3 weights
    groupIsValid g = not . null $ allPossibleGroupsWithNumElem 2 (weights `difference` g)

difference :: Eq a => [a] -> [a] -> [a]
difference a b = filter (`notElem` b) a

quantumEntanglement :: Group -> Int
quantumEntanglement = product

minimumOn :: (Ord b) => (a -> b) -> [a] -> b
minimumOn _ [] = error "Cannot compute minumum of emtpy list"
minimumOn f [a] = f a
minimumOn f (a : rs) = min (f a) (minimumOn f rs)

-- Parser

parseWeight :: Parser Weight
parseWeight = read <$> many1 digit

parseInput :: Parser [Weight]
parseInput = parseWeight `sepEndBy1` newline <* eof

part1 :: [Weight] -> IO ()
part1 = print . minimumOn quantumEntanglement . allValidGroups

part2 :: [Weight] -> IO ()
part2 = print
