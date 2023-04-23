module Day4
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String
import Data.List (partition, sort)
import Data.Char (ord, chr)

type Name = [String]

type ID = Int

type Checksum = String

data Room = Room { getName :: Name, getID :: ID, getChecksum :: Checksum } deriving (Show, Eq)

data ChecksumElem = ChecksumElem { numOccurences :: Int, checksumLetter :: Char } deriving Show

instance Eq ChecksumElem where
    (ChecksumElem n1 c1) == (ChecksumElem n2 c2) = n1 == n2 && c1 == c2

instance Ord ChecksumElem where
    (ChecksumElem n1 c1) <= (ChecksumElem n2 c2) = n1 > n2 || (n1 == n2) && (c1 < c2)

targetRoomName :: String
targetRoomName = "northpole object storage"

groupDups :: [Char] -> [ChecksumElem]
groupDups [] = []
groupDups (x : xs) =
  let (group, xs') = partition (== x) xs in ChecksumElem (length group) x : groupDups xs'

computeChecksum :: Name -> Checksum
computeChecksum = map checksumLetter . take 5 . sort . groupDups . concat

decryptName :: Room -> Name
decryptName (Room name roomId _) = map (map rotLetter) name
    where rotLetter c = chr $ ord 'a' + ((ord c - ord 'a' + roomId) `mod` 26)

isValidRoom :: Room -> Bool
isValidRoom (Room name _ checksum) = computeChecksum name == checksum

parseInput :: Parser [Room]
parseInput = many1 (parseRoom <* newline) <* eof

parseRoom :: Parser Room
parseRoom = Room <$> parseName <*> parseID <*> parseChecksum
  where
    parseName = many1 (many1 lower <* char '-')
    parseID = read <$> many1 digit
    parseChecksum = between (char '[') (char ']') $ many1 lower

part1 :: [Room] -> IO ()
part1 = print . sum . map getID . filter isValidRoom

part2 :: [Room] -> IO ()
part2 = print . getID . head . filter ((== targetRoomName) . unwords . decryptName) . filter isValidRoom
