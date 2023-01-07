module Day11
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Char (chr, ord)
import Data.List (group)
import Text.Parsec
import Text.Parsec.String

nextPassword :: String -> String
nextPassword = numToPass . (+ 1) . passToNum

passToNum :: String -> Int
passToNum = sum . zipWith (*) [26 ^ n | n <- [0 ..]] . map charToNum . reverse
  where
    charToNum = subtract (ord 'a') . ord

numToPass :: Int -> String
numToPass 0 = []
numToPass num = numToPass (num `div` 26) ++ [numToChar (num `rem` 26)]
  where
    numToChar = chr . (+) (ord 'a')

isValidPassword :: String -> Bool
isValidPassword pass =
  hasThreeConsecutiveLetters pass
    && noForbiddenLetters pass
    && hasTwoPairsOfLetters pass
  where
    hasThreeConsecutiveLetters (a : b : c : rs) =
      (ord a + 1 == ord b && ord b + 1 == ord c)
        || hasThreeConsecutiveLetters (b : c : rs)
    hasThreeConsecutiveLetters _ = False

    noForbiddenLetters = all (`notElem` "iol")

    hasTwoPairsOfLetters pass' =
      let numRepeat = map length . group $ pass'
       in any (>= 4) numRepeat || (length (filter (>= 2) numRepeat) >= 2)

nextValidPassword :: String -> String
nextValidPassword = head . dropWhile (not . isValidPassword) . iterate nextPassword . nextPassword

parseInput :: Parser String
parseInput = many1 lower

part1 :: String -> IO ()
part1 = print . nextValidPassword

part2 :: String -> IO ()
part2 = print . nextValidPassword . nextValidPassword
