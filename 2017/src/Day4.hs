module Day4
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (sort)
import Data.Set as S (Set, empty, insert, member)
import Text.Parsec
import Text.Parsec.String

type Passphrase = [String]

noDuplicateWord :: (String -> String) -> Passphrase -> Bool
noDuplicateWord f = go S.empty
  where
    go :: Set String -> Passphrase -> Bool
    go _ [] = True
    go s (p : ps) = let p' = f p in not (p' `member` s) && go (insert p' s) ps

parseInput :: Parser [Passphrase]
parseInput = (many1 alphaNum `sepBy1` char ' ') `sepEndBy1` newline <* eof

part1 :: [Passphrase] -> IO ()
part1 = print . length . filter (noDuplicateWord id)

part2 :: [Passphrase] -> IO ()
part2 = print . length . filter noAnagram
  where
    noAnagram :: Passphrase -> Bool
    noAnagram = noDuplicateWord sort
