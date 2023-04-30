module Day20
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

type IP = Int

type Range = (IP, IP)

initialUnblockedIPs :: [Range]
initialUnblockedIPs = [(0, 4294967295)]

allowedIPs :: [Range] -> [Range]
allowedIPs = foldl (flip blockRange) initialUnblockedIPs

blockRange :: Range -> [Range] -> [Range]
blockRange r = concatMap (`rangeDifference` r)

rangeDifference :: Range -> Range -> [Range]
rangeDifference (f, t) (f', t')
  | f' > t || t' < f = [(f, t)]
  | f' <= f && t' >= t = []
  | f' > f && t' < t = [(f, f' - 1), (t' + 1, t)]
  | f' <= f = [(t' + 1, t)]
  | otherwise = [(f, f' - 1)]

lowestIPNotBlocked :: [Range] -> IP
lowestIPNotBlocked ranges = minimum (map fst ranges)

numIPAllowed :: [Range] -> Int
numIPAllowed = sum . map rangeSize
  where
    rangeSize (f, t) = t - f + 1

parseInput :: Parser [Range]
parseInput = parseRange `sepEndBy1` newline <* eof

parseRange :: Parser Range
parseRange = (,) <$> parseIP <* char '-' <*> parseIP

parseIP :: Parser IP
parseIP = read <$> many1 digit

part1 :: [Range] -> IO ()
part1 = print . lowestIPNotBlocked . allowedIPs

part2 :: [Range] -> IO ()
part2 = print . numIPAllowed . allowedIPs
