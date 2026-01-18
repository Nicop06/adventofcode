module Day20
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

type Coord = (Int, Int)

data Direction = S | N | E | W deriving (Read, Show)

data Instruction = Door Direction | Branch [[Instruction]] deriving (Show)

move :: Direction -> Coord -> Coord
move W = first (subtract 1)
move E = first (+ 1)
move S = second (subtract 1)
move N = second (+ 1)

distanceFromStart :: [Instruction] -> M.Map Coord Int
distanceFromStart = go M.empty . pure . ((0, 0),0,)
  where
    go :: M.Map Coord Int -> [(Coord, Int, [Instruction])] -> M.Map Coord Int
    go m [] = m
    go m ((c, d, path) : bs) =
      let m' = M.insertWith min c d m
          d' = (M.!) m' c
       in case path of
            [] -> go m' bs
            (Door dir : is) -> go m' (bs ++ [(move dir c, d' + 1, is)])
            (Branch b : is) -> go m' (bs ++ map (c,d',) b ++ [(c, d' + 1, is)])

parseDirection :: Parser Direction
parseDirection = read . pure <$> oneOf "SNEW"

parseBranch :: Parser Instruction
parseBranch =
  Branch
    <$> between (char '(') (char ')') (many parseInstruction `sepBy1` char '|')

parseInstruction :: Parser Instruction
parseInstruction = (Door <$> parseDirection) <|> parseBranch

parseInput :: Parser [Instruction]
parseInput = between (char '^') (char '$') (many1 parseInstruction) <* newline <* eof

part1 :: [Instruction] -> IO ()
part1 = print . maximum . M.elems . distanceFromStart

part2 :: [Instruction] -> IO ()
part2 = print . length . M.filter (>= 1000) . distanceFromStart
