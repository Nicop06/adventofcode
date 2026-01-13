module Day14
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Sequence as S (Seq, fromList, index, length, (><))
import Text.Parsec (digit, many1)
import Text.Parsec.String (Parser)
import Prelude as P

initRecipes :: [Int]
initRecipes = [3, 7]

toDigitList :: Int -> [Int]
toDigitList = map (read . pure) . show

makeRecipes :: [Int]
makeRecipes = initRecipes ++ go 0 1 (fromList initRecipes)
  where
    go :: Int -> Int -> Seq Int -> [Int]
    go elf1 elf2 recipes =
      let r1 = recipes `index` elf1
          r2 = recipes `index` elf2
          score = toDigitList (r1 + r2)
          len = S.length recipes + P.length score
       in score
            ++ go
              ((elf1 + r1 + 1) `mod` len)
              ((elf2 + r2 + 1) `mod` len)
              (recipes >< fromList score)

scoreRecipe :: Int -> Int
scoreRecipe numSteps = score makeRecipes
  where
    score :: [Int] -> Int
    score = foldl (\n i -> n * 10 + i) 0 . P.take 10 . P.drop numSteps

takeUntilMatch :: [Int] -> [Int] -> [Int]
takeUntilMatch [] _ = []
takeUntilMatch (r : rs) pattern
  | and $ zipWith (==) pattern (r : rs) = []
  | otherwise = r : takeUntilMatch rs pattern

parseInput :: Parser Int
parseInput = read <$> many1 digit

part1 :: Int -> IO ()
part1 = print . scoreRecipe

part2 :: Int -> IO ()
part2 =
  print
    . P.length
    . takeUntilMatch makeRecipes
    . toDigitList
