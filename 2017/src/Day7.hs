module Day7
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow ((&&&))
import Data.Either (fromLeft)
import Data.List (groupBy, sortOn)
import Data.Map as M (Map, fromList, lookup)
import Text.Parsec
import Text.Parsec.String

data Node = Node
  { getName :: String,
    getWeight :: Int,
    getChildren :: [String]
  }
  deriving
    (Show, Eq)

parseNode :: Parser Node
parseNode =
  Node
    <$> (many1 alphaNum <* char ' ')
    <*> (read <$> between (char '(') (char ')') (many1 digit))
    <*> (string " -> " *> many1 alphaNum `sepBy1` string ", " <|> pure [])

getRoot :: [Node] -> String
getRoot nodes = case filter (not . (`elem` allChildren)) $ map getName nodes of
  [root] -> root
  rootNodes -> error $ "More than one root: " ++ show rootNodes
  where
    allChildren = foldl (\l n -> getChildren n ++ l) [] nodes

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

parseInput :: Parser [Node]
parseInput = parseNode `sepEndBy1` newline <* eof

part1 :: [Node] -> IO ()
part1 = putStrLn . getRoot

part2 :: [Node] -> IO ()
part2 nodes = print $ fromLeft 0 $ go (getNode $ getRoot nodes)
  where
    nodeMap :: Map String Node
    nodeMap = fromList . map (getName &&& id) $ nodes

    getNode :: String -> Node
    getNode n = case M.lookup n nodeMap of
      Nothing -> Node n 0 []
      Just node -> node

    go :: Node -> Either Int Int
    go (Node _ w children) = case groupOn snd $ sortOn snd $ map ((id &&& go) . getNode) children of
      -- Leaf
      [] -> Right w
      -- All sub-trees have the same weight
      [(_, Right w') : _] -> Right (w + w' * length children)
      -- Found an invalid node in one of the subtrees
      ([(_, Left w')] : _) -> Left w'
      -- One of the direct children is invalid, return the expected weight of that child.
      ws -> case sortOn length ws of
        [[(Node _ nodeW _, Right w1)], (_, Right w2) : _] -> Left (w2 - w1 + nodeW)
        _ -> error $ "More than one node not matching: " ++ show ws
