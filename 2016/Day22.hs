module Day22
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

type Coordinates = (Int, Int)

type Size = Int

type Usage = Int

data Node = Node {getCoordinates :: Coordinates, getSize :: Size, getUsed :: Size, getAvail :: Size, getUsage :: Usage} deriving (Show, Eq)

pairs :: Eq a => [a] -> [(a, a)]
pairs l = filter (uncurry (/=)) $ (,) <$> l <*> l

canMove :: Node -> Node -> Bool
canMove from to = getUsed from > 0 && getUsed from <= getAvail to

parseInput :: Parser [Node]
parseInput = count 2 skipLine *> (parseNode `sepEndBy1` newline) <* eof

skipLine :: Parser ()
skipLine = void $ many1 (noneOf "\n") <* newline

parseNode :: Parser Node
parseNode = Node <$> parseCoordinates <*> parseSize <*> parseSize <*> parseSize <*> parseUsage

parseCoordinates :: Parser Coordinates
parseCoordinates = (,) <$> (string "/dev/grid/node-x" *> parseNum) <*> (string "-y" *> parseNum <* many (char ' '))

parseSize :: Parser Size
parseSize = parseNum <* char 'T' <* many (char ' ')

parseUsage :: Parser Usage
parseUsage = parseNum <* char '%' <* many (char ' ')

parseNum :: Parser Int
parseNum = read <$> many1 digit

part1 :: [Node] -> IO ()
part1 = print . length . filter (uncurry canMove) . pairs

part2 :: [Node] -> IO ()
part2 _ = print 0
