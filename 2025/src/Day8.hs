module Day8
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (sortBy, sortOn)
import Data.Ord
import Text.Parsec
import Text.Parsec.String

type Coords = (Int, Int, Int)

distance :: Coords -> Coords -> Int
distance (x, y, z) (x', y', z') = (x' - x) ^ 2 + (y' - y) ^ 2 + (z' - z) ^ 2

allPairs :: [a] -> [(a, a)]
allPairs (x : xs) = ((x,) <$> xs) ++ allPairs xs
allPairs [] = []

closestPairs :: [Coords] -> [(Coords, Coords)]
closestPairs = sortOn (uncurry distance) . allPairs

junctionCliques :: [(Coords, Coords)] -> [[Coords]]
junctionCliques = foldr groupJunctions []

allConnected :: Int -> [[Coords]] -> Bool
allConnected n [g] = length g == n
allConnected _ _ = False

pairConnectingAll :: Int -> [[Coords]] -> [(Coords, Coords)] -> (Coords, Coords)
pairConnectingAll n gs (p : ps) =
  let gs' = groupJunctions p gs
   in if allConnected n gs'
        then p
        else pairConnectingAll n gs' ps
pairConnectingAll _ _ _ = error "No pair remaining"

findGroup :: Coords -> [[Coords]] -> ([Coords], [[Coords]])
findGroup c (g : gs)
  | c `elem` g = (g, gs)
  | otherwise =
      let (g', gs') = findGroup c gs
       in (g', g : gs')
findGroup c [] = ([c], [])

groupJunctions :: (Coords, Coords) -> [[Coords]] -> [[Coords]]
groupJunctions (c1, c2) gs =
  let (g1, gs') = findGroup c1 gs
   in if c2 `elem` g1
        then gs
        else
          let (g2, gs'') = findGroup c2 gs'
           in ((g1 ++ g2) : gs'')

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseCoords :: Parser Coords
parseCoords =
  (,,) <$> parseNumber <* char ',' <*> parseNumber <* char ',' <*> parseNumber

parseInput :: Parser [Coords]
parseInput = (parseCoords `sepEndBy1` newline) <* eof

part1 :: [Coords] -> IO ()
part1 =
  print
    . product
    . take 3
    . sortBy (comparing Down)
    . map length
    . junctionCliques
    . take 1000
    . closestPairs

part2 :: [Coords] -> IO ()
part2 cs =
  print . uncurry multXCoord $
    pairConnectingAll (length cs) [] (closestPairs cs)
  where
    multXCoord :: Coords -> Coords -> Int
    multXCoord (x, _, _) (x', _, _) = x * x'
