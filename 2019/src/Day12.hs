{-# LANGUAGE RankNTypes #-}

module Day12
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Lens
import qualified Data.Map.Strict as M
import Linear.V3
import Text.Parsec
import Text.Parsec.String

data Moon a = Moon
  { _pos :: a,
    _vel :: a
  }
  deriving (Show, Eq, Ord)

type Moon1 = Moon Int

type Moon3 = Moon (V3 Int)

dirVec :: Num a => a -> a -> a
dirVec v1 v2 = signum (v2 - v1)

updateMoons :: forall a. Num a => [Moon a] -> [Moon a]
updateMoons moons = map updateMoon moons
  where
    updateMoon moon =
      let vel = foldr ((+) . dirVec (_pos moon) . _pos) (_vel moon) moons
       in moon {_pos = _pos moon + vel, _vel = vel}

totalEnergy :: Moon3 -> Int
totalEnergy (Moon pos vel) = energy pos * energy vel
  where
    energy :: V3 Int -> Int
    energy = sum . abs

findPeriod :: forall a. (Ord a, Num a) => [Moon a] -> (Int, Int)
findPeriod = go 0 M.empty
  where
    go n m moons = case M.lookup moons m of
      Just n' -> (n', n - n')
      Nothing -> go (n + 1) (M.insert moons n m) (updateMoons moons)

newMoon :: V3 Int -> Moon3
newMoon pos = Moon pos (V3 0 0 0)

to1D :: Lens' (V3 Int) Int -> Moon3 -> Moon1
to1D l (Moon pos vel) = Moon (view l pos) (view l vel)

parseInput :: Parser [Moon3]
parseInput =
  map newMoon
    <$> (parsePosition `sepEndBy1` newline)
    <* eof
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 (digit <|> char '-')

    parsePosition :: Parser (V3 Int)
    parsePosition =
      V3
        <$> between (string "<x=") (string ", ") parseInt
        <*> between (string "y=") (string ", ") parseInt
        <*> between (string "z=") (string ">") parseInt

part1 :: [Moon3] -> IO ()
part1 = print . sum . map totalEnergy . (!! 1000) . iterate updateMoons

part2 :: [Moon3] -> IO ()
part2 moons = print $ maximum [minX, minY, minZ] + foldl1 lcm [pX, pY, pZ]
  where
    (minX, pX) = findPeriod (map (to1D _x) moons)
    (minY, pY) = findPeriod (map (to1D _y) moons)
    (minZ, pZ) = findPeriod (map (to1D _z) moons)
