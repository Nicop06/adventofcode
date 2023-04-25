module Day5
  ( parseInput,
    part1,
    part2,
  )
where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char (ord)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Text.Parsec
import Text.Parsec.String

type Password = [Maybe Char]

type Hash = String

md5Hex :: Hash -> Hash
md5Hex = B.unpack . B.toStrict . toLazyByteString . byteStringHex . hash . B.pack

generateHashes :: String -> [Hash]
generateHashes s = filter ("00000" `isPrefixOf`) [md5Hex (s ++ show i) | i <- [0 ..]]

isValidPassword :: Password -> Bool
isValidPassword = all isJust

getIndex :: Hash -> Int
getIndex s = ord (s !! 5) - ord '0'

nextPassword :: Password -> Hash -> Password
nextPassword p s = case splitAt (getIndex s) p of
  (h, c : t) -> h ++ (Just (fromMaybe (s !! 6) c) : t)
  _ -> p

generatePasswords :: String -> [Password]
generatePasswords s = scanl nextPassword initialPassword $ generateHashes s
  where
    initialPassword = replicate 8 Nothing

parseInput :: Parser String
parseInput = many1 alphaNum <* newline <* eof

part1 :: String -> IO ()
part1 = putStrLn . take 8 . map (!! 5) . generateHashes

part2 :: String -> IO ()
part2 = putStrLn . catMaybes . head . filter isValidPassword . generatePasswords
