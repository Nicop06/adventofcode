module Day3
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (nub)
import Data.Map as M (Map, elems, empty, insertWith)
import Text.Parsec
import Text.Parsec.String

data Rectangle = Rectangle
  { posX :: Int,
    posY :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show)

data Claim = Claim
  { claimId :: Int,
    rectangle :: Rectangle
  }
  deriving (Show)

claimAlloc :: [Claim] -> Map (Int, Int) [Int]
claimAlloc = foldr (\(i, c) -> insertWith (++) c [i]) M.empty . concatMap allCoords
  where
    allCoords :: Claim -> [(Int, (Int, Int))]
    allCoords (Claim i (Rectangle x y w h)) = map (i,) $ (,) <$> [x + 1 .. x + w] <*> [y + 1 .. y + h]

overlaps :: [Claim] -> [[Int]]
overlaps = filter ((> 1) . length) . elems . claimAlloc

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseRectangle :: Parser Rectangle
parseRectangle =
  Rectangle
    <$> parseInt
    <*> (char ',' *> parseInt)
    <*> (string ": " *> parseInt)
    <*> (char 'x' *> parseInt)

parseClaim :: Parser Claim
parseClaim = Claim <$> (char '#' *> parseInt) <*> (string " @ " *> parseRectangle)

parseInput :: Parser [Claim]
parseInput = parseClaim `sepEndBy1` newline <* eof

part1 :: [Claim] -> IO ()
part1 = print . length . overlaps

part2 :: [Claim] -> IO ()
part2 claims = print . head . filter (not . (`elem` claimsWithOverlap)) . map claimId $ claims
  where
    claimsWithOverlap :: [Int]
    claimsWithOverlap = nub . concat $ overlaps claims
