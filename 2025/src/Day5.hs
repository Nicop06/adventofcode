module Day5
  ( parseInput
  , part1
  , part2
  ) where

import Data.List (sortOn)
import Text.Parsec
import Text.Parsec.String

type ProductId = Int

data Range =
  Range
    { getFrom :: ProductId
    , getTo :: ProductId
    }
  deriving (Show, Eq)

data Puzzle =
  Puzzle [Range] [ProductId]
  deriving (Show, Eq)

isFresh :: [Range] -> ProductId -> Bool
isFresh [] _ = False
isFresh (Range from to:rs) p
  | p >= from && p <= to = True
  | otherwise = isFresh rs p

parseProductId :: Parser ProductId
parseProductId = read <$> many1 digit

parseRange :: Parser Range
parseRange = Range <$> parseProductId <* char '-' <*> parseProductId

parseInput :: Parser Puzzle
parseInput =
  Puzzle <$> (parseRange `sepEndBy1` newline) <* newline <*>
  (parseProductId `sepEndBy1` newline) <*
  eof

part1 :: Puzzle -> IO ()
part1 (Puzzle ranges products) =
  print . length . filter (isFresh ranges) $ products

part2 :: Puzzle -> IO ()
part2 (Puzzle ranges _) = print . numFresh . sortOn getFrom $ ranges
  where
    rangeLen (Range from to) = to - from + 1
    numFresh [] = 0
    numFresh [r] = rangeLen r
    numFresh (r1@(Range from1 to1):r2@(Range from2 to2):rs)
      | from2 <= to1 = numFresh (Range from1 (max to1 to2) : rs)
      | otherwise = rangeLen r1 + numFresh (r2 : rs)
