module Day11
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import qualified Data.Map as M
import IntCode
import Text.Parsec.String (Parser)

type Coord = (Int, Int)

type Panel = M.Map Coord Int

data Direction = U | R | D | L deriving (Enum, Show)

data RobotState = RobotState
  { _panel :: Panel,
    _pos :: Coord,
    _dir :: Direction
  }

move :: Direction -> Coord -> Coord
move U = second (+ 1)
move D = second (subtract 1)
move R = first (+ 1)
move L = first (subtract 1)

turn :: Int -> Direction -> Direction
turn 0 d = toEnum ((fromEnum d - 1) `mod` 4)
turn 1 d = toEnum ((fromEnum d + 1) `mod` 4)
turn t _ = error $ "Invalid command " ++ show t

initState :: RobotState
initState = RobotState {_panel = M.empty, _pos = (0, 0), _dir = U}

readColor :: RobotState -> Int
readColor st = M.findWithDefault 0 (_pos st) (_panel st)

robot :: RobotState -> [Int] -> [(Int, RobotState)]
robot _ [] = []
robot _ [_] = []
robot st (c : t : is) =
  let panel = M.insert (_pos st) c (_panel st)
      dir = turn t (_dir st)
      pos = move dir (_pos st)
      st' = RobotState {_panel = panel, _pos = pos, _dir = dir}
   in (readColor st', st') : robot st' is

runRobot :: Int -> [Int] -> Panel
runRobot initColor code = _panel . snd . last $ run
  where
    run :: [(Int, RobotState)]
    run = robot initState (runProgram code (initColor : map fst run))

drawPanel :: Panel -> [String]
drawPanel panel =
  [ [ toPixel $ M.findWithDefault 0 (x, y) panel
      | x <- [minX .. maxX]
    ]
    | y <- reverse [minY .. maxY]
  ]
  where
    allCoords = M.keys panel
    minX = minimum $ map fst allCoords
    minY = minimum $ map snd allCoords
    maxX = maximum $ map fst allCoords
    maxY = maximum $ map snd allCoords

    toPixel :: Int -> Char
    toPixel 0 = ' ' -- Black
    toPixel 1 = '█' -- White
    toPixel i = error $ "Unknown pixel " ++ show i

parseInput :: Parser [Int]
parseInput = parseProgram

part1 :: [Int] -> IO ()
part1 = print . length . runRobot 0

part2 :: [Int] -> IO ()
part2 = mapM_ putStrLn . drawPanel . runRobot 1
