module Day8
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array
import Text.Parsec
import Text.Parsec.String

type Pixel = Bool

type Coord = (Int, Int)

type Screen = Array Coord Pixel

data Operation = Rect Int Int | RotateRow Int Int | RotateCol Int Int deriving (Show, Eq)

screenHeight :: Int
screenHeight = 6

screenWidth :: Int
screenWidth = 50

initialScreen :: Screen
initialScreen = let b = ((0, 0), (screenHeight - 1, screenWidth - 1)) in array b [(c, False) | c <- range b]

runOperation :: Operation -> Screen -> Screen
runOperation (Rect c r) screen = screen // [(p, True) | p <- range ((0, 0), (r - 1, c - 1))]
runOperation (RotateRow r n) screen =
  screen // [((r, (y + n) `mod` screenWidth), screen ! (r, y)) | y <- [0 .. screenWidth - 1]]
runOperation (RotateCol c n) screen =
  screen // [(((x + n) `mod` screenHeight, c), screen ! (x, c)) | x <- [0 .. screenHeight - 1]]

runAllOperations :: [Operation] -> Screen
runAllOperations = foldl (flip runOperation) initialScreen

numPixelsOn :: Screen -> Int
numPixelsOn = length . filter id . elems

toPixelStrings :: Screen -> [String]
toPixelStrings screen = [[toPixel $ screen ! (i, j) | j <- [0 .. screenWidth - 1]] | i <- [0 .. screenHeight - 1]]
  where
    toPixel :: Pixel -> Char
    toPixel p = if p then '#' else '.'

parseInput :: Parser [Operation]
parseInput = parseOperation `sepEndBy1` newline <* eof

parseOperation :: Parser Operation
parseOperation = char 'r' *> (parseRect <|> parseRotate)

parseRect :: Parser Operation
parseRect = Rect <$> (string "ect " *> parseNum) <*> (char 'x' *> parseNum)

parseRotate :: Parser Operation
parseRotate = string "otate " *> (parseRotateRow <|> parseRotateCol)
  where
    parseRotateRow = RotateRow <$> (string "row y=" *> parseNum) <*> (string " by " *> parseNum)
    parseRotateCol = RotateCol <$> (string "column x=" *> parseNum) <*> (string " by " *> parseNum)

parseNum :: Parser Int
parseNum = read <$> many1 digit

part1 :: [Operation] -> IO ()
part1 = print . numPixelsOn . runAllOperations

part2 :: [Operation] -> IO ()
part2 = mapM_ putStrLn . toPixelStrings . runAllOperations
