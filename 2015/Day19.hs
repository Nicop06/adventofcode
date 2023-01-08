module Day19
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (isPrefixOf, nub, sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Text.Parsec
import Text.Parsec.String

-- Data

type Molecule = String

data Replacement = Replacement String String deriving (Show)

-- Helpers

generateMolecules :: [Replacement] -> Molecule -> [Molecule]
generateMolecules _ [] = []
generateMolecules r m =
  mapMaybe (tryReplacement m) r ++ map (head m :) (generateMolecules r (tail m))

tryReplacement :: Molecule -> Replacement -> Maybe Molecule
tryReplacement m (Replacement from to) =
  if from `isPrefixOf` m then Just $ to ++ drop (length from) m else Nothing

nextReplacement :: [Replacement] -> Molecule -> [Molecule]
nextReplacement _ [] = []
nextReplacement r m = case mapMaybe (tryReplacement m) r of
  [] -> map (head m :) $ nextReplacement r (tail m)
  m' -> m'

flipReplacement :: Replacement -> Replacement
flipReplacement (Replacement from to) = Replacement to from

sortReplacements :: [Replacement] -> [Replacement]
sortReplacements = sortOn (Down . numCharReduced)
  where
    numCharReduced (Replacement from to) = length from - length to

stepsTofabricateMolecule :: [Replacement] -> Molecule -> [Int]
stepsTofabricateMolecule r m = let r' = sortReplacements $ map flipReplacement r in
    map length $ take 1000 $ iterate (concatMap (nextReplacement r')) [m]
    --length $ takeWhile ("e" `notElem`) $ iterate (concatMap (nextReplacement r')) [m]

-- Parser

parseReplacement :: Parser Replacement
parseReplacement = Replacement <$> many1 alphaNum <*> (string " => " *> many1 alphaNum)

parseInput :: Parser ([Replacement], Molecule)
parseInput = (,) <$> parseReplacement `sepEndBy1` newline <*> (newline *> many1 alphaNum) <* newline <* eof

part1 :: ([Replacement], Molecule) -> IO ()
part1 = print . length . nub . uncurry generateMolecules

part2 :: ([Replacement], Molecule) -> IO ()
part2 = print . uncurry stepsTofabricateMolecule
