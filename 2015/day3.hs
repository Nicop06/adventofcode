import Control.Arrow (first, second)
import Data.List (nub)
import Text.Parsec
import Text.Parsec.String

data Direction = N | S | E | W deriving (Show)

type Coordinate = (Int, Int)

parseDirection :: Parser Direction
parseDirection = (N <$ char '^') <|> (S <$ char 'v') <|> (E <$ char '<') <|> (W <$ char '>')

parseInput :: IO (Either ParseError [Direction])
parseInput = parseFromFile (many1 parseDirection <* eof) "inputs/day3"

move :: Direction -> Coordinate -> Coordinate
move N = first (+ 1)
move S = first (subtract 1)
move E = second (+ 1)
move W = second (subtract 1)

visitedPlaces :: [Direction] -> [Coordinate]
visitedPlaces = scanl (flip move) (0, 0)

part1 :: [Direction] -> IO ()
part1 = print . length . nub . visitedPlaces

part2 :: [Direction] -> IO ()
part2 = print

main = parseInput >>= either print part1
