module Day7
  ( parseInput
  , part1
  , part2
  ) where

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

beamTravel :: [Tile] -> [Tile] -> [Tile]
beamTravel t t' = go (addEmpty t) (addEmpty t')
  where
    addEmpty :: [Tile] -> [Tile]
    addEmpty r = EmptySpace : r ++ [EmptySpace]
    go (t1:t2:t3:ts) (t1':t2':t3':ts') =
      let isPrevSplit = isSplit t1 t1'
          isNextSplit = isSplit t3 t3'
          numBeamsCur =
            (if isPrevSplit
               then numBeams t1
               else 0) +
            (if isNextSplit
               then numBeams t3
               else 0) +
            numBeams t2
          newTile =
            if t2' == EmptySpace && (isPrevSplit || isNextSplit || isBeam t2)
              then Beam numBeamsCur
              else t2'
       in newTile : go (t2 : t3 : ts) (newTile : t3' : ts')
    go _ _ = []

isSplit :: Tile -> Tile -> Bool
isSplit (Beam _) Splitter = True
isSplit _ _ = False

countBeamSplit :: PuzzleInput -> Int
countBeamSplit (r:r':rs) =
  let numSplit = sum . map fromEnum $ zipWith isSplit r r'
   in numSplit + countBeamSplit (beamTravel r r' : rs)
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
