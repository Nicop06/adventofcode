import Control.Applicative as A
import Data.Char (ord)
import Data.Maybe (fromMaybe, mapMaybe)
import ParseAndRun
import Text.Parsec as P
import Text.Parsec.String

-- Data

data Square = Square Char | Start | End deriving (Show, Eq)

type Row = [Square]

type Grid = [Row]

type Position = (Int, Int)

type Visited = [Position]

data Visitor = Visitor
  { currentSquare :: Square,
    currentRow :: Row,
    position :: Position,
    above :: Grid,
    below :: Grid,
    left :: Row,
    right :: Row
  }
  deriving (Show)

-- Helpers

squareHeight :: Square -> Int
squareHeight (Square c) = ord c
squareHeight Start = ord 'a'
squareHeight End = ord 'z'

canMove :: Square -> Square -> Bool
canMove from to = squareHeight to <= squareHeight from + 1

startVisiting :: Grid -> Visitor
startVisiting grid =
  Visitor
    { currentSquare = head . head $ grid,
      currentRow = head grid,
      position = (0, 0),
      above = [],
      below = tail grid,
      left = [],
      right = tail . head $ grid
    }

goRight :: Visitor -> Maybe Visitor
goRight visitor = case right visitor of
  [] -> Nothing
  s : rs ->
    Just
      visitor
        { currentSquare = s,
          position = (x + 1, y),
          left = currentSquare visitor : left visitor,
          right = rs
        }
    where
      (x, y) = position visitor

goLeft :: Visitor -> Maybe Visitor
goLeft visitor = case left visitor of
  [] -> Nothing
  s : rs ->
    Just
      visitor
        { currentSquare = s,
          position = (x - 1, y),
          left = rs,
          right = currentSquare visitor : right visitor
        }
    where
      (x, y) = position visitor

goUp :: Visitor -> Maybe Visitor
goUp visitor = case above visitor of
  [] -> Nothing
  row : a ->
    Just
      visitor
        { currentSquare = head r,
          currentRow = row,
          position = (x, y - 1),
          above = a,
          below = currentRow visitor : below visitor,
          left = reverse l,
          right = tail r
        }
    where
      (x, y) = position visitor
      (l, r) = splitAt x row

goDown :: Visitor -> Maybe Visitor
goDown visitor = case below visitor of
  [] -> Nothing
  row : b ->
    Just
      visitor
        { currentSquare = head r,
          currentRow = row,
          position = (x, y + 1),
          above = currentRow visitor : above visitor,
          below = b,
          left = reverse l,
          right = tail r
        }
    where
      (x, y) = position visitor
      (l, r) = splitAt x row

goNextRow :: Visitor -> Maybe Visitor
goNextRow visitor = case below visitor of
  [] -> Nothing
  row : b ->
    Just
      visitor
        { currentSquare = head r,
          currentRow = row,
          position = (0, y + 1),
          above = currentRow visitor : above visitor,
          below = b,
          left = l,
          right = tail r
        }
    where
      (_, y) = position visitor
      (l, r) = splitAt 0 row

goNext :: Visitor -> Maybe Visitor
goNext visitor = goRight visitor A.<|> goNextRow visitor

neighbors :: Visitor -> [Visitor]
neighbors visitor = mapMaybe ($ visitor) [goLeft, goRight, goUp, goDown]

visitableNeighbors :: (Square -> Square -> Bool) -> Visited -> Visitor -> [Visitor]
visitableNeighbors f visited visitor = filter canVisit $ neighbors visitor
  where
    canVisit el = (position el `notElem` visited) && f (currentSquare visitor) (currentSquare el)

findSquareR :: Square -> Visitor -> Maybe Visitor
findSquareR sq visitor
  | currentSquare visitor == sq = Just visitor
  | otherwise = goNext visitor >>= findSquareR sq

findSquare :: Square -> Visitor -> Visitor
findSquare sq v = fromMaybe v (findSquareR sq v)

distanceToSquareR :: Square -> (Square -> Square -> Bool) -> [([Visitor], Visitor)] -> Visited -> ([Visitor], Visitor) -> [Visitor]
distanceToSquareR sq f queue visited (path, visitor)
  | currentSquare visitor == sq = path
  | otherwise =
      let next = visitableNeighbors f visited visitor
          newQueue = queue ++ zip (repeat (visitor : path)) next
       in case newQueue of
            [] -> path
            e : r -> distanceToSquareR sq f r (map position next ++ visited) e

distanceToSquare :: Square -> (Square -> Square -> Bool) -> Visitor -> [Visitor]
distanceToSquare sq f = distanceToSquareR sq f [] [] . (,) []

-- Parser

parseSquare :: Parser Square
parseSquare = Square <$> lower P.<|> Start <$ char 'S' P.<|> End <$ char 'E'

parseGrid :: Parser Grid
parseGrid = many1 (many1 parseSquare <* newline) <* eof

part1 :: Parser Int
part1 = length . distanceToSquare End canMove . findSquare Start . startVisiting <$> parseGrid

part2 :: Parser Int
part2 = length . distanceToSquare (Square 'a') (flip canMove) . findSquare End . startVisiting <$> parseGrid

main :: IO ()
main = parseAndSolve "inputs/day12" part1 part2
