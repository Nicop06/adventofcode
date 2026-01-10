module Day8
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

data Node = Node [Node] [Int] deriving (Show)

type Tree = Node

valueOfNode :: Node -> Int
valueOfNode (Node [] metadata) = sum metadata
valueOfNode (Node children metadata) = sum $ map (valueOfNode . getChild) metadata
  where
    getChild :: Int -> Node
    getChild i
      | i <= length children = children !! (i - 1)
      | otherwise = Node [] []

parseInt :: Parser Int
parseInt = read <$> many1 digit <* spaces

parseNode :: Parser Node
parseNode = do
  numChildren <- parseInt
  numMetadata <- parseInt
  children <- count numChildren parseNode
  metadata <- count numMetadata parseInt
  return $ Node children metadata

parseInput :: Parser Tree
parseInput = parseNode <* eof

part1 :: Tree -> IO ()
part1 = print . sumMetadata
  where
    sumMetadata :: Node -> Int
    sumMetadata (Node children metadata) = sum metadata + sum (map sumMetadata children)

part2 :: Tree -> IO ()
part2 = print . valueOfNode
