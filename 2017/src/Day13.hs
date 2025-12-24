module Day13
  ( parseInput,
    part1,
    part2,
  )
where

import Data.IntMap as M (IntMap, fromList, keys, lookup)
import Text.Parsec
import Text.Parsec.String

type Firewall = IntMap Int

tripSeverity :: Int -> Firewall -> Int
tripSeverity timeShift firewall = sum $ map severity [0 .. maxLayer]
  where
    maxLayer = maximum $ keys firewall
    severity :: Int -> Int
    severity layer = case M.lookup layer firewall of
      Nothing -> 0
      Just depth ->
        let pos = layer + timeShift
         in if pos `mod` (2 * depth - 2) == 0
              then depth * pos
              else 0

calculateDelay :: Firewall -> Int
calculateDelay firewall = head . filter ((== 0) . (`tripSeverity` firewall)) $ [0 ..]

parseLayer :: Parser (Int, Int)
parseLayer = (,) <$> (parseInt <* string ": ") <*> parseInt
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 digit

parseInput :: Parser Firewall
parseInput = M.fromList <$> parseLayer `sepEndBy1` newline <* eof

part1 :: Firewall -> IO ()
part1 = print . tripSeverity 0

part2 :: Firewall -> IO ()
part2 = print . calculateDelay
