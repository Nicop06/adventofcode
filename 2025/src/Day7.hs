module Day7
  ( parseInput
  , part1
  , part2
  ) where

import Data.Array.Repa as R hiding ((++), map, zipWith)
import Data.Array.Repa.Repr.Vector as R
import Prelude as P
import Text.Parsec
import Text.Parsec.String

data Tile
  = EmptySpace
  | Splitter
  | Beam Int
  deriving (Eq, Show)

type PuzzleInput = [[Tile]]

isBeam :: Tile -> Bool
isBeam (Beam _) = True
isBeam _ = False

numBeams :: Tile -> Int
numBeams (Beam i) = i
numBeams _ = 0

newTile :: DIM1 -> (DIM1 -> Tile) -> (DIM1 -> Tile) -> DIM1 -> Tile
newTile dim f f' sh@(Z :. i) =
  let prev = (Z :. i - 1)
      next = (Z :. i + 1)
      isPrevSplit = inShape dim prev && isSplit (f prev) (f' prev)
      isNextSplit = inShape dim next && isSplit (f next) (f' next)
      numBeamsCur =
        (if isPrevSplit
           then numBeams (f prev)
           else 0) +
        (if isNextSplit
           then numBeams (f next)
           else 0) +
        numBeams (f sh)
   in if f' sh == EmptySpace && (isPrevSplit || isNextSplit || isBeam (f sh))
        then Beam numBeamsCur
        else f' sh

beamTravel :: [Tile] -> [Tile] -> [Tile]
beamTravel t t' =
  let tLen = length t
      tLen' = length t'
      dim = ix1 tLen
   in if tLen == tLen'
        then toList $
             traverse2
               (fromListVector dim t)
               (fromListVector dim t')
               (\_ _ -> dim)
               (newTile dim)
        else error $
             "Rows don't have the same length: " ++
             show tLen ++ " vs " ++ show tLen'

numSplit :: [Tile] -> [Tile] -> Int
numSplit r r' = sum . map fromEnum $ zipWith isSplit r r'

isSplit :: Tile -> Tile -> Bool
isSplit (Beam _) Splitter = True
isSplit _ _ = False

countBeamSplit :: PuzzleInput -> Int
countBeamSplit (r:r':rs) = numSplit r r' + countBeamSplit (beamTravel r r' : rs)
countBeamSplit _ = 0

parseTile :: Parser Tile
parseTile =
  (EmptySpace <$ char '.') <|> (Splitter <$ char '^') <|> (Beam 1 <$ char 'S')

parseInput :: Parser [[Tile]]
parseInput = (many1 parseTile `sepEndBy1` newline) <* eof

part1 :: PuzzleInput -> IO ()
part1 = print . countBeamSplit

part2 :: PuzzleInput -> IO ()
part2 = print . sum . map numBeams . foldl1 beamTravel
