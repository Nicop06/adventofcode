{-# LANGUAGE OverloadedStrings #-}

module Day12
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KV
import Data.Maybe (fromJust)
import Data.ByteString.Char8 (pack)
import Text.Parsec
import Text.Parsec.String

-- Helpers

sumValues :: Value -> Int
sumValues (Array v) = sum . fmap sumValues $ v
sumValues (Object obj) = sum . map (sumValues . snd) $ KV.toList obj
sumValues (Number n) = floor n
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

parseInput :: Parser Value
parseInput = fromJust . decodeStrict' . pack <$> many1 anyChar

part1 :: Value -> IO ()
part1 = print . sumValues

part2 :: Value -> IO ()
part2 = print . sumValues . skipValuesWithRed
