{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.KeyMap qualified as KV
import Data.Maybe (fromJust)
import Data.Scientific

-- Helpers

sumValues :: Value -> Int
sumValues (Array v) = sum . fmap sumValues $ v
sumValues (Object obj) = sum . map (sumValues . snd) $ KV.toList obj
sumValues (Number n) = floor $ toRealFloat n
sumValues _ = 0

skipValuesWithRed :: Value -> Value
skipValuesWithRed (Object obj) =
  if any (valueIsRed . snd) $ KV.toList obj
    then Null
    else Object (KV.map skipValuesWithRed obj)
  where
    valueIsRed (String t) = t == "red"
    valueIsRed _ = False
skipValuesWithRed (Array v) = Array (fmap skipValuesWithRed v)
skipValuesWithRed v = v

-- Parser

parseInput :: IO (Maybe Value)
parseInput = decodeFileStrict' "inputs/day12"

part1 :: Value -> IO ()
part1 = print . sumValues

part2 :: Value -> IO ()
part2 = print . sumValues . skipValuesWithRed

main :: IO ()
main = parseInput >>= part2 . fromJust
