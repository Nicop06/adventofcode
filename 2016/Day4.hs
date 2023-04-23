module Day4
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String
import Data.List (partition, sort)


type Name = String

type ID = Int

type Checksum = String

data Room = Room { getName :: Name, getID :: ID, getChecksum :: Checksum } deriving (Show, Eq)

data ChecksumElem = ChecksumElem { numOccurences :: Int, checksumLetter :: Char } deriving Show

instance Eq ChecksumElem where
    (ChecksumElem n1 c1) == (ChecksumElem n2 c2) = n1 == n2 && c1 == c2

instance Ord ChecksumElem where
    (ChecksumElem n1 c1) <= (ChecksumElem n2 c2) = n1 > n2 || (n1 == n2) && (c1 < c2)

groupDups :: [Char] -> [ChecksumElem]
groupDups [] = []
groupDups (x : xs) =
  let (group, xs') = partition (== x) xs in ChecksumElem (length group) x : groupDups xs'

computeChecksum :: Name -> Checksum
computeChecksum = map checksumLetter . take 5 . sort . groupDups

isValidRoom :: Room -> Bool
isValidRoom (Room name _ checksum) = computeChecksum name == checksum

parseInput :: Parser [Room]
parseInput = many1 (parseRoom <* newline) <* eof

parseRoom :: Parser Room
parseRoom = Room <$> parseName <*> parseID <*> parseChecksum
  where
    parseName = concat <$> many1 (many1 lower <* char '-')
    parseID = read <$> many1 digit
    parseChecksum = between (char '[') (char ']') $ many1 lower

part1 :: [Room] -> IO ()
part1 = print . sum . map getID . filter isValidRoom

part2 :: [Room] -> IO ()
part2 = part1
