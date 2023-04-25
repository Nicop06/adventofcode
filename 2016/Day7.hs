module Day7
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (isInfixOf, partition)
import Text.Parsec
import Text.Parsec.String

data IPSequence = Supernet String | Hypernet String deriving (Show, Eq)

type IP = [IPSequence]

isHypernet :: IPSequence -> Bool
isHypernet (Hypernet _) = True
isHypernet _ = False

getContent :: IPSequence -> String
getContent (Hypernet s) = s
getContent (Supernet s) = s

hasAbba :: String -> Bool
hasAbba (a : rs@(b : c : d : _)) = (a /= b && b == c && a == d) || hasAbba rs
hasAbba _ = False

listBab :: String -> [String]
listBab (a : rs@(b : c : _)) =
  let aba = listBab rs
   in if a == c && a /= b then [b, a, b] : aba else aba
listBab _ = []

isTLS :: IP -> Bool
isTLS ip =
  let (hypernet, supernet) = partition isHypernet ip
   in not (seqsHaveAbba hypernet) && seqsHaveAbba supernet
  where
    seqsHaveAbba = any (hasAbba . getContent)

isSSL :: IP -> Bool
isSSL ip =
  let (hypernet, supernet) = partition isHypernet ip
      bab = concatMap (listBab . getContent) supernet
      hasBab s = any (`isInfixOf` s) bab
   in any (hasBab . getContent) hypernet

parseInput :: Parser [IP]
parseInput = parseIP `sepEndBy1` newline <* eof

parseIP :: Parser IP
parseIP = many1 $ (Supernet <$> many1 alphaNum) <|> (Hypernet <$> between (char '[') (char ']') (many1 alphaNum))

part1 :: [IP] -> IO ()
part1 = print . length . filter isTLS

part2 :: [IP] -> IO ()
part2 = print . length . filter isSSL
