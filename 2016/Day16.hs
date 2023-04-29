module Day16
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

type Bit = Char

type Data = [Bit]

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x : rs) = if f x then [x] else x : takeUntil f rs

nextData :: Data -> Data
nextData d = d ++ "0" ++ map reverseBit (reverse d)

reverseBit :: Bit -> Bit
reverseBit '0' = '1'
reverseBit '1' = '0'
reverseBit c = error $ "Char " ++ show c ++ " is not a valid bit"

dragonCurve :: Int -> Data -> Data
dragonCurve n = take n . last . takeUntil ((>= n) . length) . iterate nextData

nextChecksum :: Data -> Data
nextChecksum (x : y : rs) = (if x == y then '1' else '0') : nextChecksum rs
nextChecksum _ = []

checksum :: Data -> Data
checksum = last . takeUntil (odd . length) . iterate nextChecksum

parseInput :: Parser Data
parseInput = many1 (oneOf "01") <* newline <* eof

finalChecksum :: Int -> Data -> Data
finalChecksum n = checksum . dragonCurve n

part1 :: Data -> IO ()
part1 = putStrLn . finalChecksum 272

part2 :: Data -> IO ()
part2 = putStrLn . finalChecksum 35651584
