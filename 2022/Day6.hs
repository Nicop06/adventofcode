module Day6
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (nub)
import Text.Parsec
import Text.Parsec.String

startOfPacket :: Int -> String -> Int
startOfPacket packetWidth stream
  | length stream < packetWidth = 0
  | startsWithMarker stream = packetWidth
  | otherwise = 1 + startOfPacket packetWidth (drop 1 stream)
  where
    startsWithMarker s = (length . nub $ take packetWidth s) == packetWidth

parseInput :: Parser String
parseInput = many1 alphaNum <* newline <* eof

part1 :: String -> IO ()
part1 = print . startOfPacket 4

part2 :: String -> IO ()
part2 = print . startOfPacket 14
