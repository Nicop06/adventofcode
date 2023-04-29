module Day19
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

type Elf = Int

stealFromLeftElf :: Int -> [[Elf]]
stealFromLeftElf numElfs = iterate steal [1 .. numElfs]
  where
    steal e =
      let e' = map snd . filter (odd . fst) . zip [1 ..] $ e
       in if odd $ length e then tail e' else e'

stealFromAcross :: Int -> Elf
stealFromAcross numElfs = stealFromAcross' 0 numElfs [1 .. numElfs]
  where
    stealFromAcross' _ _ [] = error "Cannot have a single elf left"
    stealFromAcross' _ _ [e] = e
    stealFromAcross' i n l =
      let indexToSkip = ((i + n `div` 2) `mod` n)
       in stealFromAcross' ((i + 1) `mod` n) (n - 1) (take indexToSkip l ++ drop (indexToSkip + 1) l)

parseInput :: Parser Int
parseInput = read <$> many1 digit <* newline <* eof

part1 :: Int -> IO ()
part1 = print . head . last . takeWhile (not . null) . stealFromLeftElf

part2 :: Int -> IO ()
part2 = print . stealFromAcross
