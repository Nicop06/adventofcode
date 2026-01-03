module Day20
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Function (on)
import Data.List (groupBy, minimumBy)
import Text.Parsec
import Text.Parsec.String

data Tile = Space | Vertical | Horizontal | Corner | Letter Char deriving (Show)

type Coord = (Int, Int, Int)

data Particle = Particle
  { position :: Coord,
    velosity :: Coord,
    acceleration :: Coord
  }
  deriving (Show)

parseInt :: Parser Int
parseInt = read <$> many1 (char '-' <|> digit)

distance :: Coord -> Int
distance (x, y, z) = abs x + abs y + abs z

add :: Coord -> Coord -> Coord
add (x, y, z) (x', y', z') = (x + x', y + y', z + z')

update :: Particle -> Particle
update (Particle p v a) = let v' = v `add` a in Particle (p `add` v') v' a

oneElem :: [a] -> Bool
oneElem [_] = True
oneElem _ = False

samePosition :: Particle -> Particle -> Bool
samePosition p1 p2 = position p1 == position p2

removeCollisions :: [Particle] -> [Particle]
removeCollisions = concat . filter oneElem . groupBy samePosition

parseCoord :: Parser Coord
parseCoord =
  between
    (char '<')
    (char '>')
    ( (,,)
        <$> (parseInt <* char ',')
        <*> (parseInt <* char ',')
        <*> parseInt
    )

parseParticle :: Parser Particle
parseParticle =
  Particle
    <$> (string "p=" *> parseCoord)
    <*> (string ", v=" *> parseCoord)
    <*> (string ", a=" *> parseCoord)

parseInput :: Parser [Particle]
parseInput = parseParticle `sepEndBy1` newline <* eof

part1 :: [Particle] -> IO ()
part1 = print . fst . minimumBy (compare `on` (distance . position . snd)) . zip [0 ..] . last . take 1000 . iterate (map update)

part2 :: [Particle] -> IO ()
part2 = print . length . last . take 1000 . iterate (removeCollisions . map update)
