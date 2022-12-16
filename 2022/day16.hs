import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type ValveName = String

data Valve = Valve {valveName :: ValveName, flowRate :: Int, tunnels :: [ValveName]} deriving (Show)

type Tunnels = Map.Map ValveName Valve

data ValvesState = ValvesState {openValves :: [ValveName], pathHistory :: [(ValveName, Int)], position :: ValveName} deriving (Show)

initialState :: ValvesState
initialState = ValvesState [] [] "AA"

-- Helpers

currentFlow :: Tunnels -> ValvesState -> Int
currentFlow t = sum . map (flowRate <$> (t Map.!)) . openValves

bestFlow :: [ValvesState] -> ValvesState
bestFlow = maximumBy (compare `on` totalFlow)

totalFlow :: ValvesState -> Int
totalFlow = last . scanl1 (+) . reverse . map snd . pathHistory

allPossibleMoves :: Int -> Tunnels -> ValvesState -> [ValvesState]
allPossibleMoves m t s =
  let curValve = t Map.! position s
      nextTunnel = ValvesState (openValves s) nextHistory
      curFlow = currentFlow t s
      nextFlow = totalFlow s + m * curFlow
      nextHistory = (position s, curFlow) : pathHistory s
      nextStates = map nextTunnel (tunnels curValve)
      stateWithOpenValve = ValvesState (position s : openValves s) nextHistory (position s)
   in if position s `notElem` openValves s && flowRate curValve > 0
        then stateWithOpenValve : nextStates
        else nextStates

filterBestMoves :: Tunnels -> [ValvesState] -> [ValvesState]
filterBestMoves t = map bestFlow . groupBy ((==) `on` position) . sortOn position

nextBestMoves :: Tunnels -> Int -> [ValvesState] -> [ValvesState]
nextBestMoves t m = filterBestMoves t . concatMap (allPossibleMoves m t)

runMoves :: Int -> Tunnels -> [ValvesState]
runMoves n t = foldl (flip (nextBestMoves t)) [initialState] [n, n - 1 .. 1]

runAndConcatMoves :: Int -> Tunnels -> [[ValvesState]]
runAndConcatMoves n t = foldl nextStates [[initialState]] [n, n - 1 .. 1]
  where
    nextStates s i = nextBestMoves t i (head s) : s

bestState :: Int -> Tunnels -> ValvesState
bestState n = bestFlow . runMoves n

-- Parser

parseName :: Parser ValveName
parseName = count 2 upper

parseFlow :: Parser Int
parseFlow = read <$> (string " has flow rate=" *> many1 digit)

parseTunnels :: Parser [ValveName]
parseTunnels = parseText *> parseName `sepBy` string ", "
  where
    parseText = string "; tunnel" *> optional (char 's') *> string " lead" *> optional (char 's') *> string " to valve" *> optional (char 's') *> char ' '

parseValve :: Parser Valve
parseValve = Valve <$> (string "Valve " *> parseName) <*> parseFlow <*> parseTunnels <* newline

parseInput :: Parser Tunnels
parseInput = Map.fromList . map ((,) <$> valveName <*> id) <$> many1 parseValve <* eof

part1 :: Parser Int
part1 = totalFlow . bestState 30 <$> parseInput

part2 :: Parser Int
part2 = part1

main :: IO ()
main = parseAndSolve "inputs/day16" part1 part2
