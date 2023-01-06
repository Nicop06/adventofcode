import Crypto.Hash.MD5 (hash)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Char8 qualified as B

key :: String
key = "ckczppom"

md5Hex :: String -> String
md5Hex = B.unpack . B.toStrict . toLazyByteString . byteStringHex . hash . B.pack

hashWithKey :: Int -> String
hashWithKey = md5Hex . (key ++) . show

startsWithZeros :: Int -> IO ()
startsWithZeros n = print . (+ 1) . last . takeWhile ((/= start) . take n . hashWithKey) $ [1 ..]
  where
    start = replicate n '0'

main :: IO ()
main = mapM_ startsWithZeros [5, 6]
