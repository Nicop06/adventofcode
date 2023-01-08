module Day19
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (isPrefixOf, nub)
import Data.Maybe (mapMaybe)
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

nextReplacement :: [Replacement] -> Molecule -> Molecule
nextReplacement _ [] = []
nextReplacement r m = case mapMaybe (tryReplacement m) r of
  [] -> head m : nextReplacement r (tail m)
  m' -> head m'

flipReplacement :: Replacement -> Replacement
flipReplacement (Replacement from to) = Replacement (reverse to) (reverse from)

stepsTofabricateMolecule :: [Replacement] -> Molecule -> Int
stepsTofabricateMolecule r m =
  let r' = map flipReplacement r
   in length $ takeWhile (/= "e") $ iterate (nextReplacement r') (reverse m)

-- Parser

parseReplacement :: Parser Replacement
parseReplacement = Replacement <$> many1 alphaNum <*> (string " => " *> many1 alphaNum)

parseInput :: Parser ([Replacement], Molecule)
parseInput = (,) <$> parseReplacement `sepEndBy1` newline <*> (newline *> many1 alphaNum) <* newline <* eof

part1 :: ([Replacement], Molecule) -> IO ()
part1 = print . length . nub . uncurry generateMolecules

part2 :: ([Replacement], Molecule) -> IO ()
part2 = print . uncurry stepsTofabricateMolecule
