module Day19
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

type Elf = Int

skipIndexIf :: (Int -> Bool) -> [a] -> [a]
skipIndexIf f = map snd . filter (f . fst) . zip [0 ..]

stealFromLeftElf :: Int -> [[Elf]]
stealFromLeftElf numElfs = iterate steal [1 .. numElfs]
  where
    steal e =
      let e' = skipIndexIf even e
       in if odd $ length e then tail e' else e'

stealFromAcross :: Int -> Elf
stealFromAcross numElfs = head . head . filter ((<= 1) . length) . iterate stealFromAcross' $ [1 .. numElfs]

stealFromAcross' :: [Elf] -> [Elf]
stealFromAcross' [] = error "No Elf left"
stealFromAcross' [e] = [e]
stealFromAcross' elfs =
  let elfs' = skipIndexIf (not . shouldStealFrom) elfs
      numElfStolenFrom = n - length elfs'
   in drop numElfStolenFrom elfs' ++ take numElfStolenFrom elfs'
  where
    n = length elfs
    shouldStealFrom i = i >= (n `div` 2) && (let j = (2 * i - n) `mod` 3 in j == 0 || j == 2)

parseInput :: Parser Int
parseInput = read <$> many1 digit <* newline <* eof

part1 :: Int -> IO ()
part1 = print . head . last . takeWhile (not . null) . stealFromLeftElf

part2 :: Int -> IO ()
part2 = print . stealFromAcross
