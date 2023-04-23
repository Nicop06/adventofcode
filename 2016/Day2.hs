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
nextCoord R (x, 2) = (x, 2)
nextCoord R (x, y) = (x, y + 1)
nextCoord L (x, 0) = (x, 0)
nextCoord L (x, y) = (x, y - 1)
nextCoord U (0, y) = (0, y)
nextCoord U (x, y) = (x - 1, y)
nextCoord D (2, y) = (2, y)
nextCoord D (x, y) = (x + 1, y)

nextButton :: Coordinates -> [Instruction] -> Coordinates
nextButton = foldl (flip nextCoord)

codeFromInstructions :: [[Instruction]] -> [Coordinates]
codeFromInstructions = tail . scanl nextButton (1, 1)

buttonFromCoord :: Coordinates -> Int
buttonFromCoord (x, y) = 3 * x + y + 1

parseInput :: Parser [[Instruction]]
parseInput = many1 (many1 parseInstruction <* newline)
    where parseInstruction = read . pure <$> oneOf "UDLR"

part1 :: [[Instruction]] -> IO ()
part1 = putStrLn . concatMap (show . buttonFromCoord) . codeFromInstructions

part2 :: [[Instruction]] -> IO ()
part2 = print . head
