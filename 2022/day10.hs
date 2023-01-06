import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

cyclesToAdd :: [Int]
cyclesToAdd = [20, 60, 100, 140, 180, 220]

initialValue :: Int
initialValue = 1

-- Helpers

executeCycles :: [Int -> Int] -> [Int]
executeCycles = init . scanl (flip ($)) initialValue

strengthPerCycles :: [Int] -> [Int]
strengthPerCycles = zipWith (*) [1 ..]

sumStrength :: [Int] -> Int
sumStrength l = sum [l !! (i - 1) | i <- cyclesToAdd]

drawPixel :: Int -> Int -> String
drawPixel c sprite
  | abs (col - sprite) <= 1 = '#' : newRow
  | otherwise = '.' : newRow
  where
    col = c `mod` 40
    newRow = if col == 39 then ['\n'] else ""

drawScreen :: [Int] -> String
drawScreen = concat . zipWith drawPixel [0 ..]

-- Parser

noop :: Parser [Int -> Int]
noop = [id] <$ string "noop" <* newline

addx :: Parser [Int -> Int]
addx = (\x -> [id, (+ read x)]) <$> (string "addx " *> many1 (digit <|> char '-') <* newline)

input :: Parser [Int -> Int]
input = concat <$> many1 (addx <|> noop) <* eof

part1 :: Parser Int
part1 = sumStrength . strengthPerCycles . executeCycles <$> input

part2 :: Parser String
part2 = drawScreen . executeCycles <$> input

main :: IO ()
main = parseAndSolve "inputs/day10" part1 part2
