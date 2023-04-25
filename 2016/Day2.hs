module Day2
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String

type Coordinates = (Int, Int)

data Instruction = U | D | L | R deriving (Read, Show, Eq)

nextCoord :: Instruction -> Coordinates -> Coordinates
nextCoord R (x, y) = (x, y + 1)
nextCoord L (x, y) = (x, y - 1)
nextCoord U (x, y) = (x - 1, y)
nextCoord D (x, y) = (x + 1, y)

nextButton :: (Coordinates -> Bool) -> Coordinates -> [Instruction] -> Coordinates
nextButton allowedCoord = foldl (flip nextCoord')
  where
    nextCoord' i c = let c' = nextCoord i c in if allowedCoord c' then c' else c

codeFromInstructions :: (Coordinates -> Bool) -> Coordinates -> [[Instruction]] -> [Coordinates]
codeFromInstructions allowedCoord start = tail . scanl (nextButton allowedCoord) start

parseInput :: Parser [[Instruction]]
parseInput = many1 parseInstruction `sepEndBy1` newline <* eof
  where
    parseInstruction = read . pure <$> oneOf "UDLR"

part1 :: [[Instruction]] -> IO ()
part1 = putStrLn . concatMap (show . buttonFromCoord) . codeFromInstructions allowedCoord (1, 1)
  where
    buttonFromCoord (x, y) = 3 * x + y + 1
    allowedCoord (x, y) = (x >= 0) && (y >= 0) && (x < 3) && (y < 3)

part2 :: [[Instruction]] -> IO ()
part2 = putStrLn . map buttonFromCoord . codeFromInstructions allowedCoord (2, 0)
  where
    buttonFromCoord (x, y) = let index = sum (take x [1, 3, 5, 3]) + y - ([2, 1, 0, 1, 2] !! x) in (['1' .. '9'] ++ ['A' .. 'D']) !! index
    allowedCoord (x, y) = (x + y >= 2) && (x + y <= 6) && (y - x <= 2) && (x - y <= 2)
