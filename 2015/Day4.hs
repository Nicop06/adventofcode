module Day4
  ( parseInput,
    part1,
    part2,
  )
where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Char8 as B
import Text.Parsec
import Text.Parsec.String

md5Hex :: String -> String
md5Hex = B.unpack . B.toStrict . toLazyByteString . byteStringHex . hash . B.pack

hashWithKey :: String -> Int -> String
hashWithKey key = md5Hex . (key ++) . show

startsWithZeros :: Int -> String -> IO ()
startsWithZeros n key = print . (+ 1) . last . takeWhile ((/= start) . take n . hashWithKey key) $ [1 ..]
  where
    start = replicate n '0'

parseInput :: Parser String
parseInput = many1 lower

part1 :: String -> IO ()
part1 = startsWithZeros 5

part2 :: String -> IO ()
part2 = startsWithZeros 6
