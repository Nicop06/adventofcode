module Day7
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

data IPPart = Address String | Hypernet String deriving (Show, Eq)

type IP = [IPPart]

isHypernet :: IPPart -> Bool
isHypernet (Hypernet _) = True
isHypernet _ = False

getContent :: IPPart -> String
getContent (Hypernet s) = s
getContent (Address s) = s

hasAbba :: String -> Bool
hasAbba (a : rs@(b : c : d : _)) = (a /= b && b == c && a == d) || hasAbba rs
hasAbba _ = False

isTLS :: IP -> Bool
isTLS parts = not (partsHaveAbba (filter isHypernet parts)) && partsHaveAbba (filter (not . isHypernet) parts)
  where
    partsHaveAbba = any (hasAbba . getContent)

parseInput :: Parser [IP]
parseInput = parseIP `sepEndBy1` newline <* eof

parseIP :: Parser IP
parseIP = many1 $ (Address <$> many1 alphaNum) <|> (Hypernet <$> between (char '[') (char ']') (many1 alphaNum))

part1 :: [IP] -> IO ()
part1 = print . length . filter isTLS

part2 :: [IP] -> IO ()
part2 = part1
