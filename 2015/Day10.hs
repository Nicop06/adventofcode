module Day10
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (group)
import Text.Parsec
import Text.Parsec.String

lookAndSay :: String -> String
lookAndSay = concatMap readGroup . group
  where
    readGroup = (++) <$> show . length <*> take 1

lengthLookAndSay :: Int -> String -> Int
lengthLookAndSay steps input = length (iterate lookAndSay input !! steps)

parseInput :: Parser String
parseInput = many1 digit

part1 :: String -> IO ()
part1 = print . lengthLookAndSay 40

part2 :: String -> IO ()
part2 = print . lengthLookAndSay 50
