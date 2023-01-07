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

stepsTofabricateMolecule :: [Replacement] -> Molecule -> Int
stepsTofabricateMolecule r m =
    length $ takeWhile (m `notElem`) $ iterate (nub . concatMap (generateMolecules r)) ["e"]

-- Parser

parseReplacement :: Parser Replacement
parseReplacement = Replacement <$> many1 alphaNum <*> (string " => " *> many1 alphaNum)

parseInput :: Parser ([Replacement], Molecule)
parseInput = (,) <$> parseReplacement `sepEndBy1` newline <*> (newline *> many1 alphaNum) <* newline <* eof

part1 :: ([Replacement], Molecule) -> IO ()
part1 = print . length . nub . uncurry generateMolecules

part2 :: ([Replacement], Molecule) -> IO ()
part2 = print . uncurry stepsTofabricateMolecule
