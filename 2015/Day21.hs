module Day21
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Tuple (swap)
import Text.Parsec
import Text.Parsec.String

-- Data

data CharacterType = Boss | Player deriving (Show, Enum, Eq)

data Stats = Stats {getDamage :: Int, getArmor :: Int} deriving (Show)

data Item = Item {itemName :: String, itemCost :: Int, itemStats :: Stats} deriving (Show)

data Character = Character {characterType :: CharacterType, getHP :: Int, getStats :: Stats} deriving (Show)

weapons :: [Item]
weapons =
  [ Item "Dagger" 8 (Stats 4 0),
    Item "Shortsword" 10 (Stats 5 0),
    Item "Warhammer" 25 (Stats 6 0),
    Item "Longsword" 40 (Stats 7 0),
    Item "Greataxe" 74 (Stats 8 0)
  ]

armors :: [Item]
armors =
  [ Item "Leather" 13 (Stats 0 1),
    Item "Chainmail" 31 (Stats 0 2),
    Item "Splintmail" 53 (Stats 0 3),
    Item "Bandedmail" 75 (Stats 0 4),
    Item "Platemail" 102 (Stats 0 5)
  ]

rings :: [Item]
rings =
  [ Item "Damage +1" 25 (Stats 1 0),
    Item "Damage +2" 50 (Stats 2 0),
    Item "Damage +3" 100 (Stats 3 0),
    Item "Defense +1" 20 (Stats 0 1),
    Item "Defense +2" 40 (Stats 0 2),
    Item "Defense +3" 80 (Stats 0 3)
  ]

initialPlayer :: Character
initialPlayer = Character Player 100 (Stats 0 0)

-- Helpers

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

applyItem :: Item -> Character -> Character
applyItem (Item _ _ (Stats d a)) (Character t hp (Stats d' a')) =
  Character t hp (Stats (d + d') (a + a'))

possibleItems :: [[Item]]
possibleItems = map concat $ sequence [weaponOpt, armorOpt, ringsOpt]
  where
    weaponOpt = map (: []) weapons
    armorOpt = [] : map (: []) armors
    ringsOpt = filter ((<= 2) . length) $ subsets rings

totalCost :: [Item] -> Int
totalCost = sum . map itemCost

playerWithItems :: [Item] -> Character
playerWithItems = foldr applyItem initialPlayer

isAlive :: Character -> Bool
isAlive c = getHP c > 0

dealDamage :: (Character, Character) -> (Character, Character)
dealDamage (attacker, defender) =
  let damageDealt = getDamage (getStats attacker) - getArmor (getStats defender)
   in (attacker, defender {getHP = getHP defender - damageDealt})

fightWinner :: Character -> Character -> CharacterType
fightWinner player boss =
  characterType . snd . head . dropWhile (isAlive . fst) . iterate (swap . dealDamage) $ (player, boss)

itemsWithWinner :: CharacterType -> Character -> [[Item]]
itemsWithWinner winner boss =
  filter ((== winner) . flip fightWinner boss . playerWithItems) possibleItems

-- Parser

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseHP :: Parser Int
parseHP = string "Hit Points: " *> parseNumber <* newline

parseDamage :: Parser Int
parseDamage = string "Damage: " *> parseNumber <* newline

parseArmor :: Parser Int
parseArmor = string "Armor: " *> parseNumber <* newline

parseInput :: Parser Character
parseInput = Character Boss <$> parseHP <*> (Stats <$> parseDamage <*> parseArmor)

part1 :: Character -> IO ()
part1 = print . minimum . map totalCost . itemsWithWinner Player

part2 :: Character -> IO ()
part2 = print . maximum . map totalCost . itemsWithWinner Boss
