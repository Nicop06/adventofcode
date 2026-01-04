module Day21
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (nub, transpose)
import Data.Map.Strict as M (Map, fromList, lookup)
import Text.Parsec
import Text.Parsec.String

type Pixel = Char

type Pattern = [[Pixel]]

type Image = [[Pixel]]

type Rule = (Pattern, Pattern)

type Rules = Map Pattern Pattern

initialPattern :: Pattern
initialPattern =
  [ ".#.",
    "..#",
    "###"
  ]

allPatterns :: Pattern -> [Pattern]
allPatterns p =
  nub $
    (.)
      <$> [id, transpose]
      <*> [id, reverse]
      <*> [p, map reverse p]

enhanceImage :: Rules -> Image -> Image
enhanceImage rules image = reshape $ go image
  where
    patternSize :: Int
    patternSize = if even (length image) then 2 else 3

    numPatternPerRow :: Int
    numPatternPerRow = length image `div` patternSize

    reshape :: [Pattern] -> Image
    reshape [] = []
    reshape p = foldr1 (zipWith (++)) (take numPatternPerRow p) ++ reshape (drop numPatternPerRow p)

    go :: Image -> [Pattern]
    go [] = []
    go ([] : rs) = go rs
    go l =
      let p = map (take patternSize) $ take patternSize l
       in case M.lookup p rules of
            Nothing -> error $ "Cannot find pattern " ++ show p
            Just res -> res : go (map (drop patternSize) (take patternSize l) ++ drop patternSize l)

expandRules :: Rule -> [Rule]
expandRules (from, to) = (,to) <$> allPatterns from

numPixels :: Int -> Rules -> Int
numPixels numIter rules =
  length
    . concatMap (filter (== '#'))
    . last
    . take (numIter + 1)
    . iterate (enhanceImage rules)
    $ initialPattern

parsePattern :: Parser Pattern
parsePattern = many1 (char '.' <|> char '#') `sepBy1` char '/'

parseRule :: Parser Rule
parseRule = (,) <$> (parsePattern <* string " => ") <*> parsePattern

parseInput :: Parser Rules
parseInput = fromList . concatMap expandRules <$> parseRule `sepEndBy1` newline <* eof

part1 :: Rules -> IO ()
part1 = print . numPixels 5

part2 :: Rules -> IO ()
part2 = print . numPixels 18
