module Day11
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Applicative ((<**>))
import Control.Arrow (first, second)
import Data.List (groupBy, sort, sortOn)
import Data.Set qualified as S
import Text.Parsec
import Text.Parsec.String

type Fuel = String

data Part = Microship Fuel | Generator Fuel deriving (Show, Eq, Ord)

data Floor = Floor {floorName :: String, floorContent :: [Part]} deriving (Show, Eq, Ord)

type FloorState = ([Floor], [Floor])

type CacheItem = (Int, [(Int, Int)])

data CachedState = CachedState {floorStates :: [FloorState], statesCache :: S.Set CacheItem} deriving (Show, Eq)

extraParts :: [Part]
extraParts = [Generator, Microship] <*> ["elerium", "dilithium"]

cacheItem :: FloorState -> CacheItem
cacheItem (d, u) =
  let floorContents = map floorContent $ reverse d ++ u
      allMicroships = concatMap microships floorContents
      getIndex' [] _ _ = -1
      getIndex' (f : rs) i p = if p `elem` f then i else getIndex' rs (i + 1) p
      getIndex = getIndex' floorContents 0
   in (length d, sort [(getIndex (Microship f), getIndex (Generator f)) | f <- allMicroships])

nextFloorStates :: FloorState -> [FloorState]
nextFloorStates (_, []) = []
nextFloorStates (d, (Floor n p) : u) = concat $ [uncurry goUp, uncurry goDown] <*> map (second newFloorState) (splitN 1 p ++ splitN 2 p)
  where
    newFloorState p' = (d, Floor n p' : u)

allNextStates :: CachedState -> CachedState
allNextStates (CachedState states cache) =
  let nextStates = concatMap nextFloorStates states
      statesAndCacheItem = zip nextStates (map cacheItem nextStates)
      filteredStates = filter (not . (`S.member` cache) . snd) . map head . groupBy sameCachedElem . sortOn snd $ statesAndCacheItem
      sameCachedElem s s' = snd s == snd s'
   in CachedState (map fst filteredStates) (cache `S.union` S.fromList (map snd filteredStates))

splitN :: Int -> [a] -> [([a], [a])]
splitN 0 l = [([], l)]
splitN _ [] = []
splitN n (x : rs) = map (first (x :)) (splitN (n - 1) rs) ++ map (second (x :)) (splitN n rs)

isSubset :: Ord a => [a] -> [a] -> Bool
isSubset xs ys = all (`elem` ys) xs

arePartsSafe :: [Part] -> Bool
arePartsSafe p =
  let m = microships p
      g = generators p
   in null g || m `isSubset` g

microships :: [Part] -> [Fuel]
microships [] = []
microships (Microship f : rs) = f : microships rs
microships (_ : rs) = microships rs

generators :: [Part] -> [Fuel]
generators [] = []
generators (Generator f : rs) = f : generators rs
generators (_ : rs) = generators rs

goUp :: [Part] -> FloorState -> [FloorState]
goUp _ (_, []) = []
goUp _ (_, [_]) = []
goUp p (d, f : (Floor n p') : u) =
  let p'' = p ++ p'
   in [(f : d, Floor n p'' : u) | arePartsSafe p'']

goDown :: [Part] -> FloorState -> [FloorState]
goDown _ ([], _) = []
goDown p ((Floor n p') : d, u) =
  let p'' = p ++ p'
   in [(d, Floor n p'' : u) | arePartsSafe p'']

isFinalState :: FloorState -> Bool
isFinalState (f, [_]) = all (null . floorContent) f
isFinalState _ = False

allFloorStates :: [Floor] -> [CachedState]
allFloorStates f = takeWhile (not . any isFinalState . floorStates) . iterate allNextStates $ initialStates
  where
    initialStates = CachedState [([], f)] S.empty

parseInput :: Parser [Floor]
parseInput = parseFloor `sepEndBy1` newline <* eof

parseFloor :: Parser Floor
parseFloor = Floor <$> parseFloorName <*> parseParts

parseFloorName :: Parser String
parseFloorName = string "The " *> many1 alphaNum <* string " floor contains "

parseParts :: Parser [Part]
parseParts = ([] <$ string "nothing relevant.") <|> many1 parsePart <* char '.'

parsePart :: Parser Part
parsePart = parsePrefix *> many1 alphaNum <**> (parseMicroship <|> parseGenerator) <* parseSuffix
  where
    parsePrefix = string "a" *> optional (string "nd a") *> char ' '
    parseSuffix = many (oneOf ", ")

parseMicroship :: Parser (Fuel -> Part)
parseMicroship = Microship <$ string "-compatible microchip"

parseGenerator :: Parser (Fuel -> Part)
parseGenerator = Generator <$ string " generator"

part1 :: [Floor] -> IO ()
part1 = print . length . allFloorStates

part2 :: [Floor] -> IO ()
part2 ((Floor n p) : rs) = part1 (Floor n (p ++ extraParts) : rs)
part2 [] = part1 []
