module Day14
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Maybe (fromJust)
import Data.Sequence as S
import Text.Parsec
import Text.Parsec.String
import Prelude as P

type GameState = (Int, Int, Seq Int)

initState :: GameState
initState = (0, 1, fromList [3, 7])

recipeScores :: Int -> Seq Int
recipeScores = fromList . map (read . pure) . show

next :: GameState -> GameState
next (elf1, elf2, recipes) =
  ( (elf1 + r1 + 1) `mod` S.length recipes',
    (elf2 + r2 + 1) `mod` S.length recipes',
    recipes'
  )
  where
    r1 = fromJust $ recipes !? elf1
    r2 = fromJust $ recipes !? elf2
    recipes' = recipes >< recipeScores (r1 + r2)

makeRecipes :: Int -> Int
makeRecipes numSteps = score (recipes !! (numSteps + 10))
  where
    recipes = iterate next initState

    score :: (a, b, Seq Int) -> Int
    score (_, _, l) = foldl (\n i -> n * 10 + i) 0 $ S.take 10 $ S.drop numSteps l

parseInput :: Parser Int
parseInput = read <$> many1 digit

part1 :: Int -> IO ()
part1 = print . makeRecipes . const 9

part2 :: Int -> IO ()
part2 _ = mapM_ print $ P.take 10 $ iterate next initState
