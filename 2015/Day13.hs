module Day13
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (groupBy, permutations, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

-- Data

type Person = String

type Happiness = Int

type TablePreferences = M.Map Person [(Person, Happiness)]

-- Helpers

buildPreferences :: [(Person, (Person, Happiness))] -> TablePreferences
buildPreferences = M.fromList . map personPreference . groupBy samePerson . sortOn fst
  where
    samePerson (p1, _) (p2, _) = p1 == p2
    personPreference people = (fst . head $ people, map snd people)

totalHappiness :: TablePreferences -> [Person] -> Happiness
totalHappiness preferences = sum . map amountHappiness . seatingPair
  where
    amountHappiness (p1, p2) =
      let happiness = fromJust $ M.lookup p1 preferences
       in snd . head . filter ((== p2) . fst) $ happiness

seatingPair :: [Person] -> [(Person, Person)]
seatingPair l =
  (head l, last l) : (last l, head l) : seatingPairR l
  where
    seatingPairR [] = []
    seatingPairR [_] = []
    seatingPairR (a : b : rs) = (a, b) : (b, a) : seatingPairR (b : rs)

allArrangementHappiness :: TablePreferences -> [Int]
allArrangementHappiness preferences = map (totalHappiness preferences) . permutations $ M.keys preferences

addMyself :: TablePreferences -> TablePreferences
addMyself m = M.insert "Me" (map (,0) (M.keys m)) (M.map (("Me", 0) :) m)

-- Parser

parsePerson :: Parser Person
parsePerson = many1 alphaNum

parseHappiness :: Parser Happiness
parseHappiness = parseSign <*> (read <$> many1 digit) <* string " happiness units"
  where
    parseSign = (id <$ string "gain ") <|> ((* (-1)) <$ string "lose ")

parseInput :: Parser TablePreferences
parseInput = buildPreferences <$> (parsePrefence `sepEndBy1` newline) <* eof
  where
    parsePrefence = (,) <$> (parsePerson <* string " would ") <*> parseNeighborHappiness
    parseNeighborHappiness = flip (,) <$> parseHappiness <*> (string " by sitting next to " *> parsePerson <* char '.')

part1 :: TablePreferences -> IO ()
part1 = print . maximum . allArrangementHappiness

part2 :: TablePreferences -> IO ()
part2 = print . maximum . allArrangementHappiness . addMyself
