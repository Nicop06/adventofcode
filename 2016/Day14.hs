module Day14
  ( parseInput,
    part1,
    part2,
  )
where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Char8 qualified as B
import Data.List (isInfixOf, group)
import Text.Parsec
import Text.Parsec.String

type Hash = String

md5Hex :: Hash -> Hash
md5Hex = B.unpack . B.toStrict . toLazyByteString . byteStringHex . hash . B.pack

generateHashes :: String -> [(Int, Hash)]
generateHashes s = [(i, md5Hex (s ++ show i)) | i <- [0 ..]]

isValidPassword :: [Hash] -> Bool
isValidPassword [] = False
isValidPassword (h : rs) = case triplets h of
    (c : _) : _ -> any (replicate 5 c `isInfixOf`) $ take 1000 (tail rs)
    _ -> False

triplets :: Hash -> [String]
triplets = filter ((>=3) . length) . group

parseInput :: Parser String
parseInput = many1 alphaNum <* newline <* eof

part1 :: String -> IO ()
part1 = print . fst . head . last . take 64 . filter (isValidPassword . map snd) . iterate tail . generateHashes

part2 :: String -> IO ()
part2 = putStrLn
