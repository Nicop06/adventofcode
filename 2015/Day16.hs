module Day16
  ( parseInput,
    part1,
    part2,
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String

-- Data

type Samples = M.Map String Int

data AuntSue = AuntSue {auntNumber :: Int, auntKnownInfo :: Samples} deriving (Show)

samples :: Samples
samples =
  M.fromList
    [ ("children", 3),
      ("cats", 7),
      ("samoyeds", 2),
      ("pomeranians", 3),
      ("akitas", 0),
      ("vizslas", 0),
      ("goldfish", 5),
      ("trees", 3),
      ("cars", 2),
      ("perfumes", 1)
    ]

-- Helpers

auntSueMatchesSamples :: (String -> Int -> Int -> Bool) -> AuntSue -> Bool
auntSueMatchesSamples f (AuntSue _ info) = and $ M.intersectionWithKey f info knownSamples
  where
    knownSamples = M.restrictKeys samples (S.fromList $ M.keys info)

-- Parser

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseInfo :: Parser Samples
parseInfo = M.fromList <$> parseOneInfo `sepEndBy1` string ", "
  where
    parseOneInfo = (,) <$> (many1 alphaNum <* string ": ") <*> parseNumber

parseAuntSue :: Parser AuntSue
parseAuntSue = AuntSue <$> (string "Sue " *> parseNumber <* string ": ") <*> parseInfo

parseInput :: Parser [AuntSue]
parseInput = parseAuntSue `sepEndBy1` newline <* eof

part1 :: [AuntSue] -> IO ()
part1 = print . auntNumber . head . filter (auntSueMatchesSamples (const (==)))

part2 :: [AuntSue] -> IO ()
part2 = print . auntNumber . head . filter (auntSueMatchesSamples matchProperty)
  where
    matchProperty "cats" = (>)
    matchProperty "trees" = (>)
    matchProperty "pomeranians" = (<)
    matchProperty "goldfish" = (<)
    matchProperty _ = (==)
