module Day10
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Array
import Text.Parsec
import Text.Parsec.String

type Coord = (Int, Int)

data Point = Point {position :: Coord, velocity :: Coord} deriving (Show)

updatePosition :: Point -> Point
updatePosition (Point (x, y) (vx, vy)) = Point (x + vx, y + vy) (vx, vy)

getBounds :: [Point] -> (Coord, Coord)
getBounds points =
  let minX = minimum $ map (fst . position) points
      minY = minimum $ map (snd . position) points
      maxX = maximum $ map (fst . position) points
      maxY = maximum $ map (snd . position) points
   in ((minX, minY), (maxX, maxY))

toLightMap :: [Point] -> [String]
toLightMap points =
  let b@((minX, minY), (maxX, maxY)) = getBounds points
      arr = array b [(c, ' ') | c <- range b] // [(position p, '#') | p <- points]
   in [[arr ! (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]

getArea :: [Point] -> Int
getArea points =
  let ((minX, minY), (maxX, maxY)) = getBounds points
   in (maxX - minX) * (maxY - minY)

parsePoint :: Parser Point
parsePoint = do
  skipStrings
  x <- parseInt
  skipStrings
  y <- parseInt
  skipStrings
  vx <- parseInt
  skipStrings
  vy <- parseInt <* char '>'
  return $ Point (x, y) (vx, vy)
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 (digit <|> char '-')

    skipStrings :: Parser ()
    skipStrings = skipMany (noneOf $ ['0' .. '9'] ++ "-")

parseInput :: Parser [Point]
parseInput = parsePoint `sepEndBy1` newline <* eof

printLights :: [Point] -> IO ()
printLights points = do
  mapM_ putStrLn $ toLightMap points
  putStrLn ""

part1 :: [Point] -> IO ()
part1 = printLights . head . filter ((< 1000) . getArea) . iterate (map updatePosition)

part2 :: [Point] -> IO ()
part2 = print . length . takeWhile ((> 1000) . getArea) . iterate (map updatePosition)
