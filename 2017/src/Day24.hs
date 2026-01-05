module Day24
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Text.Parsec
import Text.Parsec.String

type Component = (Int, Int)

data Bridge = Bridge Int [Component] deriving (Show)

expandBridge :: Bridge -> [Component] -> [(Bridge, [Component])]
expandBridge (Bridge port bridge) = go
  where
    go :: [Component] -> [(Bridge, [Component])]
    go [] = []
    go (c@(x, y) : cs)
      | x == port = (Bridge y (c : bridge), cs) : continue c cs
      | y == port = (Bridge x (c : bridge), cs) : continue c cs
      | otherwise = continue c cs

    continue :: Component -> [Component] -> [(Bridge, [Component])]
    continue c cs = map (second (c :)) (go cs)

allBridges :: [Component] -> [Bridge]
allBridges components =
  map fst
    . concat
    . takeWhile (not . null)
    . iterate (concatMap (uncurry expandBridge))
    $ [(Bridge 0 [], components)]

strength :: Bridge -> Int
strength (Bridge _ bridge) = sum $ map (uncurry (+)) bridge

bridgeLength :: Bridge -> Int
bridgeLength (Bridge _ l) = length l

parseInput :: Parser [Component]
parseInput = parseComponent `sepEndBy1` newline <* eof
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 digit

    parseComponent :: Parser Component
    parseComponent = (,) <$> (parseInt <* char '/') <*> parseInt

part1 :: [Component] -> IO ()
part1 = print . maximum . map strength . allBridges

part2 :: [Component] -> IO ()
part2 components = print . maximum . map strength . filter ((== longestBridgeLen) . bridgeLength) $ bridges
  where
    bridges = allBridges components
    longestBridgeLen = maximum $ map bridgeLength bridges
