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
skipIndexIf f = map snd . filter (not . f . fst) . zip [0 ..]

{-
Stealing from the left means that each Elf with an odd position will be
stolen from. If the size is odd, the last Elf to be stolen from is the first
Elf.
-}
stealFromLeftElf :: [Elf] -> Elf
stealFromLeftElf [] = error "No Elf left to play"
stealFromLeftElf [e] = e
stealFromLeftElf elfs =
  let elfs' = skipIndexIf odd elfs
   in stealFromLeftElf (if odd $ length elfs then tail elfs' else elfs')

{-
When stealing from across, each Elf will steal from the Elf at index n / 2,
rounded down, where n corresponds to the number of remaining Elfs. When it's
the Elf at position i to steal, it will steal from the Elf at position (2 * i +
(N - i) / 2, rounded down, where N is the total number of Elfs in the list.
This is because the length of the list shrinks by one, and because one Elf is
eliminated at each step, all the indices move left, which means the next Elf to
steal from is one step further in the original list.

The way to solve the problem is to recursively filter out Elfs that match this
condition. We find that only Elfs in the second half of the list (including the
Elf in the middle for list of odd size) can be eliminated. Using the formula
from above, we need to find Elfs with index k that is such that k = floor((3 *
i + N) / 2), where i is any index in the list. This condition is satisfied if
and only if k >= floor(n / 2) and (2 * k - n) `mod` 3 is 0 or (-1).

Once we filter out the Elfs that were stolen from, we need to count how many
Elfs played, move them at the end of the list and repeat the process until only
one Elf is left.

This runs in O(n * log(n)) as we eliminate about one third of the Elfs at each
round. A naive solution would run in O(n^2), which would take too much time to
terminate.
-}
stealFromAcross :: [Elf] -> Elf
stealFromAcross [] = error "No Elf left to play"
stealFromAcross [e] = e
stealFromAcross elfs =
  let elfs' = skipIndexIf shouldStealFrom elfs
      numElfStolenFrom = n - length elfs'
   in stealFromAcross $ drop numElfStolenFrom elfs' ++ take numElfStolenFrom elfs'
  where
    n = length elfs
    shouldStealFrom k = k >= (n `div` 2) && (let j = (2 * k - n) `mod` 3 in j == 0 || j == 2)

parseInput :: Parser Int
parseInput = read <$> many1 digit <* newline <* eof

part1 :: Int -> IO ()
part1 numElfs = print . stealFromLeftElf $ [1 .. numElfs]

part2 :: Int -> IO ()
part2 numElfs = print . stealFromAcross $ [1 .. numElfs]
