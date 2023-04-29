module Day14
  ( parseInput,
    part1,
    part2,
  )
where

import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Char8 qualified as B
import Data.List (group, isInfixOf)
import Text.Parsec
import Text.Parsec.String

type Hash = String

md5Hex :: String -> Hash
md5Hex = B.unpack . B.toStrict . toLazyByteString . byteStringHex . MD5.hash . B.pack

generateHashes :: (Int -> Hash) -> [(Int, Hash)]
generateHashes hash = [(i, hash i) | i <- [0 ..]]

hashFunction :: Int -> String -> Int -> Hash
hashFunction n s i = last . take (n + 1) . iterate md5Hex $ (s ++ show i)

isValidPassword :: [Hash] -> Bool
isValidPassword [] = False
isValidPassword (h : rs) = case triplets h of
  (c : _) : _ -> any (replicate 5 c `isInfixOf`) $ take 1000 (tail rs)
  _ -> False

triplets :: Hash -> [String]
triplets = filter ((>= 3) . length) . group

parseInput :: Parser String
parseInput = many1 alphaNum <* newline <* eof

indexToGenerateHashNumber :: Int -> String -> Int
indexToGenerateHashNumber n = fst . head . last . take 64 . filter (isValidPassword . map snd) . iterate tail . generateHashes . hashFunction n

part1 :: String -> IO ()
part1 = print . indexToGenerateHashNumber 1

part2 :: String -> IO ()
part2 = print . indexToGenerateHashNumber 2017
