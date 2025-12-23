module Day12
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (nub)
import Data.Map as M (Map, findWithDefault, fromListWith, keys, mapWithKey)
import Data.Set as S (Set, difference, elems, empty, fromList, insert, member, size)
import Text.Parsec
import Text.Parsec.String

data Pipe = Pipe Int [Int] deriving (Show, Eq)

type Graph = Map Int [Int]

makeGraph :: [Pipe] -> Graph
makeGraph = mapWithKey removeDuplicates . fromListWith (++) . concatMap allConnections
  where
    allConnections :: Pipe -> [(Int, [Int])]
    allConnections (Pipe from to) = (from, to) : map (,[from]) to

    removeDuplicates :: Int -> [Int] -> [Int]
    removeDuplicates from = filter (/= from) . nub

connectedComponent :: Int -> Graph -> Set Int
connectedComponent start graph = go [start] S.empty
  where
    go :: [Int] -> Set Int -> Set Int
    go [] visited = visited
    go (x : xs) visited = go (xs ++ children) (insert x visited)
      where
        children = filter (not . (`member` visited)) $ M.findWithDefault [] x graph

numConnectedComponents :: Graph -> Int
numConnectedComponents graph = go (S.fromList $ keys graph)
  where
    go :: Set Int -> Int
    go toVisit = case elems toVisit of
      [] -> 0
      (el : _) -> 1 + go (difference toVisit (connectedComponent el graph))

parsePipe :: Parser Pipe
parsePipe = Pipe <$> (parseInt <* string " <-> ") <*> (parseInt `sepBy1` string ", ")
  where
    parseInt = read <$> many1 digit

parseInput :: Parser Graph
parseInput = makeGraph <$> parsePipe `sepEndBy1` newline <* eof

part1 :: Graph -> IO ()
part1 = print . size . connectedComponent 0

part2 :: Graph -> IO ()
part2 = print . numConnectedComponents
