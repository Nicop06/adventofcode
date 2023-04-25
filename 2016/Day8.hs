module Day8
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array.Repa as A
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

type Pixel = Bool

type Screen = Array U DIM2 Pixel

type UpdateFunc = DIM2 -> (DIM2 -> Pixel) -> DIM2 -> Pixel

data Operation = Rect Int Int | RotateRow Int Int | RotateCol Int Int deriving (Show, Eq)

screenHeight :: Int
screenHeight = 6

screenWidth :: Int
screenWidth = 50

screenSize :: DIM2
screenSize = ix2 screenWidth screenHeight

initialScreen :: Screen
initialScreen = fromJust $ computeUnboxedP $ fromFunction screenSize (const False)

applyUpdate :: UpdateFunc -> Screen -> Screen
applyUpdate update l = fromJust $ computeUnboxedP $ A.traverse l id (update $ extent l)

runOperation :: Operation -> UpdateFunc
runOperation (Rect c r) _ f idx@(Z :. x :. y) = f idx || (x < c && y < r)
runOperation (RotateRow r n) (Z :. sx :. _) f idx@(Z :. x :. y) =
  if y == r then f (Z :. (x + n) `mod` sx :. y) else f idx
runOperation (RotateCol c n) (Z :. _ :. sy) f idx@(Z :. x :. y) =
  if x == c then f (Z :. x :. (y + n) `mod` sy) else f idx

runAllOperations :: [Operation] -> Screen
runAllOperations = foldr (applyUpdate . runOperation) initialScreen

numPixelsOn :: Screen -> Int
numPixelsOn = length . filter id . toList

toPixelStrings :: Screen -> [String]
toPixelStrings = reshapeList screenWidth . toList . A.map toPixel
    where toPixel p = if p then '#' else '.'

reshapeList :: Int -> [a] -> [[a]]
reshapeList _ [] = []
reshapeList n l = let (start, end) = splitAt n l in start : reshapeList n end

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
