module Day5
  ( parseInput,
    part1,
    part2,
  )
where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Char8 qualified as B
import Data.List (isPrefixOf)
import Text.Parsec
import Text.Parsec.String

md5Hex :: String -> String
md5Hex = B.unpack . B.toStrict . toLazyByteString . byteStringHex . hash . B.pack

generateHashes :: String -> [String]
generateHashes s = [md5Hex (s ++ show i) | i <- [0 ..]]

generatePassword :: String -> String
generatePassword = map (!! 5) . filter ("00000" `isPrefixOf`) . generateHashes

parseInput :: Parser String
parseInput = many1 alphaNum <* newline <* eof

part1 :: String -> IO ()
part1 = putStrLn . take 8 . generatePassword

part2 :: String -> IO ()
part2 = print . md5Hex
