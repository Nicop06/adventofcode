module Day13
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.Array as A
import Data.List (sort)
import Data.Map as M (Map, delete, fromList, insert, keys, lookup, member)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Text.Parsec hiding (Empty, getPosition)
import Text.Parsec.String
import Prelude

type Coord = (Int, Int)

data Turn
  = TurnLeft
  | GoStraight
  | TurnRight
  deriving (Show)

data Direction
  = West
  | North
  | East
  | South
  deriving (Show, Enum)

data Tile
  = Empty
  | Track Char
  | Corner Char
  | Intersection
  deriving (Show)

data ParsedTile
  = TrackTile Tile
  | CartTile Direction
  deriving (Show)

type TrackMap = Array Coord Tile

data Cart = Cart Direction Turn deriving (Show)

type Carts = Map Coord Cart

data Puzzle = Puzzle TrackMap Carts

nextTurn :: Turn -> Turn
nextTurn TurnLeft = GoStraight
nextTurn GoStraight = TurnRight
nextTurn TurnRight = TurnLeft

turnDirection :: Turn -> Direction -> Direction
turnDirection GoStraight d = d
turnDirection TurnLeft d = case d of
  West -> South
  _ -> pred d
turnDirection TurnRight d = case d of
  South -> West
  _ -> succ d

turnCorner :: Char -> Direction -> Direction
turnCorner '/' North = East
turnCorner '/' South = West
turnCorner '/' East = North
turnCorner '/' West = South
turnCorner '\\' North = West
turnCorner '\\' South = East
turnCorner '\\' East = South
turnCorner '\\' West = North
turnCorner c _ = error $ "Invalid corner " ++ show c

nextCoord :: Direction -> Coord -> Coord
nextCoord East = first (+ 1)
nextCoord West = first (subtract 1)
nextCoord North = second (subtract 1)
nextCoord South = second (+ 1)

updateDirection :: Tile -> Cart -> Cart
updateDirection tile (Cart dir turn) = case tile of
  Empty -> error "Cart on empty tile"
  Track _ -> Cart dir turn
  Corner c -> Cart (turnCorner c dir) turn
  Intersection -> Cart (turnDirection turn dir) (nextTurn turn)

tilesToPuzzle :: [[ParsedTile]] -> Puzzle
tilesToPuzzle l = Puzzle trackMap carts
  where
    h = length l
    w = length $ head l
    b = ((0, 0), (w - 1, h - 1))
    tileAndCoords = zip (map swap $ range ((0, 0), (h - 1, w - 1))) (concat l)

    carts :: Carts
    carts = fromList $ mapMaybe toCart tileAndCoords

    trackMap :: TrackMap
    trackMap = array b (map (second parsedTileToTile) tileAndCoords)

    parsedTileToTile :: ParsedTile -> Tile
    parsedTileToTile (CartTile d) = case d of
      South -> Track '|'
      North -> Track '|'
      East -> Track '-'
      West -> Track '-'
    parsedTileToTile (TrackTile t) = t

    toCart :: (Coord, ParsedTile) -> Maybe (Coord, Cart)
    toCart (c, CartTile d) = Just (c, Cart d TurnLeft)
    toCart _ = Nothing

moveAllCarts :: TrackMap -> Map Coord Cart -> (Maybe Coord, Map Coord Cart)
moveAllCarts m carts = go (sort $ keys carts) carts
  where
    go :: [Coord] -> Map Coord Cart -> (Maybe Coord, Map Coord Cart)
    go [] cartMap = (Nothing, cartMap)
    go (p : ps) cartMap = case M.lookup p cartMap of
      Nothing -> go ps cartMap
      Just c@(Cart dir _) ->
        let p' = nextCoord dir p
            c' = updateDirection (m ! p') c
         in if member p' cartMap
              then (Just p', snd $ go ps (delete p' (delete p cartMap)))
              else go ps (insert p' c' (delete p cartMap))

_printMap :: TrackMap -> Carts -> [String]
_printMap m carts = [[printTileOrCart (x, y) | x <- range (minX, maxX)] | y <- range (minY, maxY)]
  where
    ((minX, minY), (maxX, maxY)) = bounds m
    printTileOrCart :: Coord -> Char
    printTileOrCart c = case M.lookup c carts of
      Just (Cart South _) -> 'v'
      Just (Cart North _) -> '^'
      Just (Cart East _) -> '>'
      Just (Cart West _) -> '<'
      Nothing -> printTile (m ! c)

    printTile :: Tile -> Char
    printTile Empty = ' '
    printTile (Track c) = c
    printTile (Corner c) = c
    printTile Intersection = '+'

parseDirection :: Parser Direction
parseDirection =
  (North <$ char '^')
    <|> (South <$ char 'v')
    <|> (East <$ char '>')
    <|> (West <$ char '<')

parseTile :: Parser ParsedTile
parseTile =
  (TrackTile . Track <$> oneOf "|-")
    <|> (TrackTile . Corner <$> oneOf "\\/")
    <|> (TrackTile Empty <$ char ' ')
    <|> (TrackTile Intersection <$ char '+')
    <|> (CartTile <$> parseDirection)

parseMap :: Parser [[ParsedTile]]
parseMap = many1 parseTile `sepEndBy1` newline

parseInput :: Parser Puzzle
parseInput = tilesToPuzzle <$> parseMap

part1 :: Puzzle -> IO ()
part1 (Puzzle m carts) =
  print
    . head
    . mapMaybe fst
    . iterate (moveAllCarts m . snd)
    $ (Nothing, carts)

part2 :: Puzzle -> IO ()
part2 (Puzzle m carts) =
  print
    . head
    . keys
    . head
    . filter ((<= 1) . length)
    . iterate (snd . moveAllCarts m)
    $ carts
