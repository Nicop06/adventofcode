module Day13
  ( parseInput,
    part1,
    part2,
  )
where

import qualified Data.Map as M
import Data.Maybe
import IntCode
import Text.Parsec.String

type Coord = (Int, Int)

data Tile
  = Empty
  | Wall
  | Block
  | Paddle
  | Ball
  deriving (Show, Enum, Eq)

type Screen = M.Map Coord Tile

tilePos :: Tile -> Screen -> Maybe Coord
tilePos t = listToMaybe . M.keys . M.filter (== t)

simulateGame :: [Int] -> [Int] -> [(Int, Screen)]
simulateGame prog = go 0 M.empty . runProgram prog
  where
    go :: Int -> Screen -> [Int] -> [(Int, Screen)]
    go score screen [] = [(score, screen)]
    go score screen (x : y : t : rs)
      | (x, y) == (-1, 0) = go t screen rs
      | otherwise =
          let screen' = M.insert (x, y) (toEnum t) screen
              tile = toEnum t
           in case tile of
                -- Input is requested after each update of the ball.
                Ball -> (score, screen') : go score screen' rs
                _ -> go score screen' rs
    go _ _ input = error $ "Invalid input " ++ show input

playGame :: [Int] -> Int
playGame prog = fst . last $ simulator
  where
    simulator = simulateGame (2 : tail prog) (joystick simulator)

joystick :: [(Int, Screen)] -> [Int]
joystick [] = []
joystick ((_, screen) : rs) =
  case (tilePos Ball screen, tilePos Paddle screen) of
    (Just (ballX, _), Just (paddleX, _)) -> signum (ballX - paddleX) : joystick rs
    _ -> 0 : joystick rs

parseInput :: Parser [Int]
parseInput = parseProgram

part1 :: [Int] -> IO ()
part1 prog = print . length . M.filter (== Block) . snd . last $ simulateGame prog []

part2 :: [Int] -> IO ()
part2 prog = print $ playGame (2 : tail prog)
