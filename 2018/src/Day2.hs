module Day2
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (group, sort)
import Text.Parsec
import Text.Parsec.String

allPairs :: [a] -> [(a, a)]
allPairs (l : ls) = map (l,) ls ++ allPairs ls
allPairs [] = []

matchingElems :: Eq a => [a] -> [a] -> [a]
matchingElems l1 l2 = map fst . filter (uncurry (==)) $ zip l1 l2

hasNRepeats :: Int -> String -> Bool
hasNRepeats n = any ((== n) . length) . group . sort

parseInput :: Parser [String]
parseInput = many1 alphaNum `sepEndBy1` newline <* eof

part1 :: [String] -> IO ()
part1 ids = print (n * m)
  where
    n = length $ filter (hasNRepeats 2) ids
    m = length $ filter (hasNRepeats 3) ids

part2 :: [String] -> IO ()
part2 ids = mapM_ putStrLn . filter ((== n - 1) . length) $ map (uncurry matchingElems) (allPairs ids)
  where
    n = length (head ids)
