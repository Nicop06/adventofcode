module Day9
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

data Token = Text Int | Marker Int Int deriving (Show, Eq)

tokenLen :: Token -> Int
tokenLen (Text n) = n
tokenLen (Marker n m) = 3 + length (show n) + length (show m)

decompress :: [Token] -> [Token]
decompress [] = []
decompress ((Text n) : rs) = Text n : decompress rs
decompress ((Marker n r) : rs) = let (e, rs') = expandTokens n rs in concat (replicate r e) ++ decompress rs'

fullDecompressedSize :: [Token] -> Int
fullDecompressedSize [] = 0
fullDecompressedSize ((Text n) : rs) = n + fullDecompressedSize rs
fullDecompressedSize ((Marker n r) : rs) = let (e, rs') = expandTokens n rs in (r * fullDecompressedSize e) + fullDecompressedSize rs'

expandTokens :: Int -> [Token] -> ([Token], [Token])
expandTokens _ [] = ([], [])
expandTokens n (h : ts)
  | n == tokenLen h = ([h], ts)
  | n < tokenLen h = ([Text n], Text (tokenLen h - n) : ts)
  | otherwise = let (e, ts') = expandTokens (n - tokenLen h) ts in (h : e, ts')

parseInput :: Parser [Token]
parseInput = many1 (parseMarker <|> parseText) <* newline <* eof

parseText :: Parser Token
parseText = Text . length <$> many1 (noneOf "(\n")

parseNum :: Parser Int
parseNum = read <$> many1 digit

parseMarker :: Parser Token
parseMarker = Marker <$> (char '(' *> parseNum <* char 'x') <*> parseNum <* char ')'

part1 :: [Token] -> IO ()
part1 = print . sum . map tokenLen . decompress

part2 :: [Token] -> IO ()
part2 = print . fullDecompressedSize
