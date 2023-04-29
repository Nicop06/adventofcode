module Day15
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

type Time = Int

type Position = Int

type Id = Int

data Disc = Disc { discId :: Id, numPositions :: Position, initialPosition :: Position } deriving (Show, Eq)

discPositionForTime :: Time -> Disc -> Position
discPositionForTime time (Disc dId numPos initPos) = (time + initPos + dId) `mod` numPos

allDiscPositionsForTime :: Time -> [Disc] -> [Position]
allDiscPositionsForTime t = map (discPositionForTime t)

allDiscPositions :: [Disc] -> [[Position]]
allDiscPositions d = map (`allDiscPositionsForTime` d) [0..]

addNewDisc :: [Disc] -> [Disc]
addNewDisc d = d ++ [Disc (discId (last d) + 1) 11 0]

parseInput :: Parser [Disc]
parseInput = parseDisc `sepEndBy1` newline <* eof

parseDisc :: Parser Disc
parseDisc = Disc <$> parseDiscId <*> parseNumPos <*> parseInitPos
    where parseDiscId = string "Disc #" *> parseNum
          parseNumPos = string " has " *> parseNum <* string " positions; "
          parseInitPos = string "at time=0, it is at position " *> parseNum <* char '.'

parseNum :: Parser Int
parseNum = read <$> many1 digit

part1 :: [Disc] -> IO ()
part1 = print . length . takeWhile (any (/= 0)) . allDiscPositions

part2 :: [Disc] -> IO ()
part2  = part1 . addNewDisc
