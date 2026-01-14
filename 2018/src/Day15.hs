module Day15
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.Array as A
import Data.List (sort)
import Data.Map as M (Map, assocs, delete, elems, empty, fromList, insert, insertWith, keys, lookup, map, member)
import Data.Maybe
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String
import Prelude as P

data Direction = South | North | East | West deriving (Show, Enum, Bounded)

data UnitType = Goblin | Elf deriving (Show, Eq)

data Unit = Unit {unitType :: UnitType, health :: Int} deriving (Show)

data Tile = Wall | Space deriving (Show)

data InputTile = MapTile Tile | UnitTile UnitType deriving (Show)

type Coord = (Int, Int)

type CaveMap = Array Coord Tile

type Units = Map Coord Unit

data Puzzle = Puzzle CaveMap Units deriving (Show)

initHitPoints :: Int
initHitPoints = 200

enemy :: UnitType -> UnitType
enemy Elf = Goblin
enemy Goblin = Elf

move :: Direction -> Coord -> Coord
move North = second (subtract 1)
move South = second (+ 1)
move West = first (subtract 1)
move East = first (+ 1)

isUnitType :: UnitType -> Unit -> Bool
isUnitType t (Unit t' _) = t == t'

isSpace :: Tile -> Bool
isSpace Space = True
isSpace _ = False

adjacentTiles :: CaveMap -> Coord -> [Coord]
adjacentTiles cave coord =
  sort
    . filter (isSpace . (cave !))
    . filter (inRange (bounds cave))
    $ move <$> [minBound ..] <*> [coord]

secondToLast :: [a] -> Maybe a
secondToLast [] = Nothing
secondToLast [_] = Nothing
secondToLast [x, _] = Just x
secondToLast (_ : xs) = secondToLast xs

maybeMin :: Ord a => [a] -> Maybe a
maybeMin [] = Nothing
maybeMin l = Just (minimum l)

targets :: Units -> UnitType -> [Coord]
targets units t =
  P.map fst $
    filter (isUnitType (enemy t) . snd) $
      M.assocs units

nextCoord :: CaveMap -> Units -> (Coord, Unit) -> Maybe Coord
nextCoord cave units (start, Unit t _) =
  maybeMin
    . mapMaybe secondToLast
    . allPaths
    . snd
    =<< closestTarget destinations
  where
    destinations :: [Coord]
    destinations =
      concatMap
        ( filter
            (not . (`M.member` units))
            . adjacentTiles cave
        )
        (targets units t)

    closestTarget :: [Coord] -> Maybe (Int, Coord)
    closestTarget [] = Nothing
    closestTarget (c : cs) = case M.lookup c parents of
      Nothing -> closestTarget cs
      Just (d, _) -> case closestTarget cs of
        Nothing -> Just (d, c)
        Just (d', c') -> Just $ min (d, c) (d', c')

    allPaths :: Coord -> [[Coord]]
    allPaths c =
      if c == start
        then [[c]]
        else case M.lookup c parents of
          Nothing -> [[c]]
          Just (_, p) -> P.map (c :) $ concatMap allPaths p

    parents :: Map Coord (Int, [Coord])
    parents = go M.empty S.empty [(0, start)]

    go :: Map Coord (Int, [Coord]) -> S.Set Coord -> [(Int, Coord)] -> Map Coord (Int, [Coord])
    go m _ [] = m
    go m s ((d, c) : cs)
      | c `S.member` s = go m s cs
      | M.member c units && c /= start = go m s cs
      | otherwise =
          let adj = adjacentTiles cave c
              m' = foldr (\c' -> insertWith addCoord c' (d + 1, [c])) m adj
           in go m' (S.insert c s) (cs ++ P.map (d + 1,) adj)

    addCoord :: (Int, [Coord]) -> (Int, [Coord]) -> (Int, [Coord])
    addCoord (d1, c1) (d2, c2)
      | d1 == d2 = (d1, c1 ++ c2)
      | d1 < d2 = (d1, c1)
      | otherwise = (d2, c2)

turn :: CaveMap -> Int -> Units -> (Units, Bool)
turn cave elfAttack = uncurry go . (M.keys &&& id)
  where
    attackPower :: UnitType -> Int
    attackPower Goblin = 3
    attackPower Elf = elfAttack

    attack :: (Coord, Unit) -> Units -> Maybe Units
    attack (coord, Unit t _) units =
      let t' = enemy t
       in case sort
            . P.map (first health)
            . filter (isUnitType t' . fst)
            $ mapMaybe (\c -> (,c) <$> M.lookup c units) (adjacentTiles cave coord) of
            (hp, coord') : _ ->
              let hp' = hp - attackPower t
               in if hp' <= 0
                    then Just $ M.delete coord' units
                    else Just $ M.insert coord' (Unit t' hp') units
            [] -> Nothing

    unitTurn :: (Coord, Unit) -> Units -> Maybe Units
    unitTurn (coord, u) units = case targets units (unitType u) of
      [] -> Nothing
      _ -> case attack (coord, u) units of
        Nothing -> case nextCoord cave units (coord, u) of
          Nothing -> Just units
          Just c ->
            let units' = M.insert c u (M.delete coord units)
             in Just $ fromMaybe units' (attack (c, u) units')
        Just units' -> Just units'

    go :: [Coord] -> Units -> (Units, Bool)
    go [] units = (units, True)
    go (c : cs) units = case M.lookup c units of
      Nothing -> go cs units
      Just unit -> case unitTurn (c, unit) units of
        Nothing -> (units, False)
        Just units' -> go cs units'

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x : xs)
  | f x = [x]
  | otherwise = x : takeUntil f xs

gameOutcome :: [(Units, Bool)] -> Int
gameOutcome turns = (length (filter snd turns) - 1) * totalHealth (fst $ last turns)
  where
    totalHealth :: Units -> Int
    totalHealth units = sum $ M.map health units

runGame :: Int -> Puzzle -> [(Units, Bool)]
runGame elfAttack (Puzzle cave units) =
  takeUntil (not . snd) $
    iterate (turn cave elfAttack . fst) (units, True)

parseMap :: Parser [[InputTile]]
parseMap = many1 parseTile `sepEndBy1` newline <* eof
  where
    parseTile :: Parser InputTile
    parseTile =
      (MapTile Wall <$ char '#')
        <|> (MapTile Space <$ char '.')
        <|> (UnitTile Goblin <$ char 'G')
        <|> (UnitTile Elf <$ char 'E')

makePuzzle :: [[InputTile]] -> Puzzle
makePuzzle tiles = Puzzle caveMap units
  where
    makeUnit :: InputTile -> Maybe Unit
    makeUnit (UnitTile t) = Just (Unit t initHitPoints)
    makeUnit _ = Nothing

    h = length tiles
    w = length $ head tiles
    b = ((1, 1), (h, w))
    tilesAndCoords = zip (range b) (concat tiles)

    caveMap :: CaveMap
    caveMap = array b (P.map (second replaceUnitTile) tilesAndCoords)

    units :: Units
    units = fromList $ mapMaybe (\(c, t) -> (c,) <$> makeUnit t) tilesAndCoords

    replaceUnitTile :: InputTile -> Tile
    replaceUnitTile (UnitTile _) = Space
    replaceUnitTile (MapTile t) = t

_toAscii :: CaveMap -> Units -> [String]
_toAscii cave units = [[coordToAscii (x, y) | y <- [minY .. maxY]] | x <- [minX .. maxX]]
  where
    ((minX, minY), (maxX, maxY)) = bounds cave
    coordToAscii :: Coord -> Char
    coordToAscii c = case M.lookup c units of
      Nothing -> tileToAscii (cave ! c)
      Just u -> unitToAscii u

    unitToAscii :: Unit -> Char
    unitToAscii (Unit Elf _) = 'E'
    unitToAscii (Unit Goblin _) = 'G'

    tileToAscii :: Tile -> Char
    tileToAscii Wall = '#'
    tileToAscii Space = '.'

_showGame :: CaveMap -> [Units] -> IO ()
_showGame cave =
  mapM_
    ( \(i, u) ->
        mapM_
          putStrLn
          ( ("Round " ++ show i) : show (totalHealth u) : _toAscii cave u
          )
    )
    . zip [0 ..]
  where
    totalHealth :: Units -> [(Coord, Int)]
    totalHealth units = P.map (second health) (M.assocs units)

parseInput :: Parser Puzzle
parseInput = makePuzzle <$> parseMap

part1 :: Puzzle -> IO ()
part1 =
  print
    . gameOutcome
    . runGame 3

part2 :: Puzzle -> IO ()
part2 p@(Puzzle _ units) = print (go 3)
  where
    numElfs :: Units -> Int
    numElfs = length . filter (isUnitType Elf) . M.elems

    numInitElfs :: Int
    numInitElfs = numElfs units

    go :: Int -> Int
    go elfAttack =
      let game = takeWhile ((== numInitElfs) . numElfs . fst) (runGame elfAttack p)
       in if snd (last game)
            then go (elfAttack + 1)
            else gameOutcome game
