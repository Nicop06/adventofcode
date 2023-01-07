module Day13
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (elemIndex, sort)
import Text.Parsec
import Text.Parsec.String

-- Data

data Packet = Packet [Packet] | Value Int deriving (Show)

type Pair = (Packet, Packet)

divider1 :: Packet
divider1 = Packet [Packet [Value 2]]

divider2 :: Packet
divider2 = Packet [Packet [Value 6]]

dividers :: [Packet]
dividers = [divider1, divider2]

-- Helpers

instance Eq Packet where
  (Value a) == (Value b) = a == b
  (Value a) == (Packet b) = Packet [Value a] == Packet b
  (Packet a) == (Value b) = Packet a == Packet [Value b]
  (Packet a) == (Packet b) = a == b

instance Ord Packet where
  (Value a) <= (Value b) = a <= b
  (Value a) <= (Packet b) = Packet [Value a] <= Packet b
  (Packet a) <= (Value b) = Packet a <= Packet [Value b]
  (Packet a) <= (Packet b) = a <= b

isRightOrder :: Pair -> Bool
isRightOrder = uncurry (<)

pairsInRightOrder :: [Pair] -> [Int]
pairsInRightOrder = map fst . filter (isRightOrder . snd) . zip [1 ..]

allPackets :: [Pair] -> [Packet]
allPackets [] = []
allPackets ((a, b) : rs) = a : b : allPackets rs

packetIndex :: Packet -> [Packet] -> Int
packetIndex p l = maybe 0 (+ 1) (p `elemIndex` l)

dividerIndices :: [Packet] -> [Int]
dividerIndices p =
  let sortedPackets = sort (dividers ++ p)
   in map (`packetIndex` sortedPackets) dividers

-- Parser

parseValue :: Parser Packet
parseValue = Value . read <$> many1 digit

parsePacket :: Parser Packet
parsePacket = Packet <$> between (char '[') (char ']') ((parseValue <|> parsePacket) `sepBy` char ',')

parsePair :: Parser Pair
parsePair = ((,) <$> parsePacket) <*> (newline *> parsePacket) <* newline

parseInput :: Parser [Pair]
parseInput = (parsePair `sepBy1` newline) <* eof

part1 :: [Pair] -> IO ()
part1 = print . sum . pairsInRightOrder

part2 :: [Pair] -> IO ()
part2 = print . product . dividerIndices . allPackets
