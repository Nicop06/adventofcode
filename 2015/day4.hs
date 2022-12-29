import Crypto.Hash.MD5
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Char8 as B

key = "ckczppom"

md5Hex :: String -> String
md5Hex = B.unpack . B.toStrict . toLazyByteString . byteStringHex . hash . B.pack

hashWithKey :: Int -> String
hashWithKey = md5Hex . (key ++) . show

startsWithZeros :: Int -> IO ()
startsWithZeros n = print . (+1) . last . takeWhile ((/=start) . take n . hashWithKey) $ [1..]
    where start = replicate n '0'
