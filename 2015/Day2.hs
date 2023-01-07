module Day2
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (sort)
import Text.Parsec
import Text.Parsec.String

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseDimensions :: Parser [Int]
parseDimensions = parseNumber `sepBy1` char 'x'

parseInput :: Parser [[Int]]
parseInput = parseDimensions `sepEndBy1` newline <* eof

wrappingPaperSurface :: [Int] -> Int
wrappingPaperSurface [l, w, h] =
  let sideSurface = [l * w, l * h, w * h]
      smallestSide = minimum sideSurface
   in smallestSide + sum (map (* 2) sideSurface)
wrappingPaperSurface d = error $ "Invalid number of dimensions: " ++ show (length d)

ribbonLength :: [Int] -> Int
ribbonLength = (+) <$> product <*> (* 2) . sum . take 2 . sort

part1 :: [[Int]] -> IO ()
part1 = print . sum . map wrappingPaperSurface

part2 :: [[Int]] -> IO ()
part2 = print . sum . map ribbonLength
