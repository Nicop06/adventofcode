import Control.Arrow (first, second)
import Data.Set qualified as S
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type Position = (Int, Int)

type Elfs = S.Set Position

data Direction = N | S | W | E deriving (Show, Eq)

directions :: [Direction]
directions = [N, S, W, E]

-- Helpers

move :: Direction -> Position -> Position
move N = first (subtract 1)
move S = first (+ 1)
move E = second (+ 1)
move W = second (subtract 1)

isFree :: Elfs -> Direction -> Position -> Bool
isFree elfs dir pos = all (`S.notMember` elfs) posToCheck
  where
    posToCheck
      | dir == N || dir == S = [move E pos, move W pos, pos]
      | otherwise = [move N pos, move S pos, pos]

maybeMove :: [Direction] -> Elfs -> Position -> Position
maybeMove dir elfs pos =
  let possiblePos = zip dir $ fmap move dir <*> [pos]
   in case filter (uncurry $ isFree elfs) possiblePos of
        [] -> pos
        [_, _, _, _] -> pos
        d : _ -> snd d

moveAllElfsOnce :: ([Direction], Elfs) -> ([Direction], Elfs)
moveAllElfsOnce (dir, elfs) =
  let oldAndNewPos = map ((,) <$> id <*> maybeMove dir elfs) $ S.toList elfs
      allNewPos = map snd oldAndNewPos
      oldOrNew (old, new) = if (> 1) . length $ filter (== new) allNewPos then old else new
   in (tail dir ++ [head dir], S.fromList $ map oldOrNew oldAndNewPos)

moveAllElfs :: Int -> Elfs -> Elfs
moveAllElfs n elfs = snd . last . take (n + 1) $ iterate moveAllElfsOnce (directions, elfs)

roundNoMove :: Elfs -> Int
roundNoMove elfs = roundNoMoveR (directions, elfs) 1
  where
    roundNoMoveR dirAndElfs n =
      let dirAndElfs' = moveAllElfsOnce dirAndElfs
       in if null (snd dirAndElfs' `S.difference` snd dirAndElfs)
            then n
            else roundNoMoveR dirAndElfs' (n + 1)

rectangleCovered :: Elfs -> (Position, Position)
rectangleCovered = (,) <$> minAndMax . S.map fst <*> minAndMax . S.map snd
  where
    minAndMax = (,) <$> minimum <*> maximum

numEmptyGroundTiles :: Elfs -> Int
numEmptyGroundTiles elfs =
  let ((minX, maxX), (minY, maxY)) = rectangleCovered elfs
   in (maxX - minX + 1) * (maxY - minY + 1) - length elfs

-- Parser

parseRow :: Parser [Int]
parseRow = map fst . filter ((== '#') . snd) . zip [0 ..] <$> many1 (char '#' <|> char '.')

parseInput :: Parser Elfs
parseInput = S.fromList . concat . zipWith (\x -> fmap (x,)) [0 ..] <$> parseRow `sepEndBy1` newline <* eof

part1 :: Parser Int
part1 = numEmptyGroundTiles . moveAllElfs 10 <$> parseInput

part2 :: Parser Int
part2 = roundNoMove <$> parseInput

main :: IO ()
main = parseAndSolve "inputs/day23" part1 part2
