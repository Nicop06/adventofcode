import Data.List (elemIndex)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type Coordinate = Int

type Index = Int

type Size = Int

type IndexedCoordinate = (Index, Coordinate)

data Status = Pending | Processed deriving (Show, Eq)

data Zipper = Zipper { left :: [Coordinate], right :: [(Status, Coordinate)] } deriving (Show, Eq)

-- Helpers

modulo :: Int -> Int -> Int
modulo a b = let m = a `mod` b in if m < 0 then m + b else m

insertAt :: a -> Index -> [a] -> [a]
insertAt el i ls = let (l, r) = splitAt i ls in l ++ (el : r)

computeIndex :: Coordinate -> Size -> Index -> Index
computeIndex c s i = (i + c) `modulo` s

processOneItem :: Zipper -> Zipper
processOneItem z@(Zipper l []) = z
processOneItem z@(Zipper l ((st,el):r))
    | st == Processed = Zipper (l ++ [el]) r
    | newIndex < lenL = Zipper (insertAt el newIndex l) r
    | otherwise = Zipper l (insertAt (Processed, el) (newIndex - lenL) r)
    where totalLen = length l + length r
          lenL = length l
          lenR = length r
          newIndex = computeIndex el totalLen lenL

performMixing :: [Coordinate] -> [Zipper]
performMixing = updatePosition . Zipper [] . zip (repeat Pending)
    where updatePosition z@(Zipper l []) = [z]
          updatePosition z = z : (updatePosition $ processOneItem z)

mixingResult :: [Coordinate] -> [Coordinate]
mixingResult = left . last . performMixing

groveCoordinate :: [Coordinate] -> Int
groveCoordinate ls =
    let mixed = mixingResult ls
        len = length ls
        Just zeroIndex = elemIndex 0 mixed
        in sum $ map ((mixed!!) . computeIndex zeroIndex len) [1000, 2000, 3000]

-- Parser

number :: Parser Int
number = read <$> many1 (char '-' <|> digit)

parseInput :: Parser [Int]
parseInput = number `sepEndBy1` newline <* eof

part1 :: Parser Int
part1 = groveCoordinate <$> parseInput

{-part2 :: Parser Int-}
part2 = parseInput

main :: IO ()
main = parseAndSolve "inputs/day20" part1 part2
