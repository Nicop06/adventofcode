import Data.List (elemIndex, findIndex)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type Coordinate = Int

type Index = Int

type Size = Int

type IndexedCoordinate = (Index, Coordinate)

decryptionKey = 811589153 :: Int

-- Helpers

modulo :: Int -> Int -> Int
modulo a b = let m = a `mod` b in if m < 0 then m + b else m

insertAt :: a -> Index -> [a] -> [a]
insertAt el i ls = let (l, r) = splitAt i ls in l ++ (el : r)

computeIndex :: Coordinate -> Size -> Index -> Index
computeIndex c s i = (i + c) `modulo` s

performMixingR :: Index -> [IndexedCoordinate] -> [[IndexedCoordinate]]
performMixingR idx ls
    | idx == length ls = [ls]
    | otherwise = ls : performMixingR (idx+1) newList
    where Just pos = findIndex ((==idx) . fst) ls
          (l, el:r) = splitAt pos ls
          pos' = computeIndex (snd el) (length ls - 1) pos
          newList = if pos' < pos then (insertAt el pos' l) ++ r else l ++ (insertAt el (pos' - pos) r)

performMixing :: Int -> [Coordinate] -> [[IndexedCoordinate]]
performMixing times ls = take (times + 1) $ iterate (last . performMixingR 0) (zip [0..] ls)
{-performMixing times ls = [last . performMixingR 0 $ zip [0..] ls]-}

mixingResult :: Int -> [Coordinate] -> [Coordinate]
mixingResult times = map snd . last . performMixing times

groveCoordinate :: Int -> [Coordinate] -> Int
groveCoordinate times ls =
    let mixed = mixingResult times ls
        len = length ls
        Just zeroIndex = elemIndex 0 mixed
        in sum $ map ((mixed!!) . computeIndex zeroIndex len) [1000, 2000, 3000]

applyDecriptionKey :: [Coordinate] -> [Coordinate]
applyDecriptionKey = map (*decryptionKey)

-- Parser

number :: Parser Int
number = read <$> many1 (char '-' <|> digit)

parseInput :: Parser [Int]
parseInput = number `sepEndBy1` newline <* eof

part1 :: Parser Int
part1 = groveCoordinate 1 <$> parseInput

part2 :: Parser Int
part2 = groveCoordinate 10 . applyDecriptionKey <$> parseInput

main :: IO ()
main = parseAndSolve "inputs/day20" part1 part2
