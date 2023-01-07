module Day16() where
import Data.Function
import Data.List (maximumBy, groupBy, sortOn)
import Data.Map.Strict qualified as Map
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type ValveName = String

data Valve = Valve {valveName :: ValveName, flowRate :: Int, tunnels :: [ValveName]} deriving (Show)

type Tunnels = Map.Map ValveName Valve

data ValvesState = ValvesState {openValves :: [ValveName], position :: ValveName} deriving (Show)

type CombinedPosition = [ValveName]

data CombinedState = CombinedState [ValveName] [(CombinedPosition, Int)] CombinedPosition deriving (Show)

-- Helpers

currentFlow :: Tunnels -> [ValveName] -> Int
currentFlow t = sum . map (flowRate <$> (t Map.!))

bestFlow :: [CombinedState] -> CombinedState
bestFlow = maximumBy (compare `on` totalFlow)

totalFlow :: CombinedState -> Int
totalFlow = last . scanl1 (+) . reverse . map snd . pathHistory
  where
    pathHistory (CombinedState _ h _) = h

allPossibleMoves :: Tunnels -> ValvesState -> [ValvesState]
allPossibleMoves t s =
  let curValve = t Map.! position s
      nextTunnel = ValvesState (openValves s)
      nextStates = map nextTunnel (tunnels curValve)
      stateWithOpenValve = ValvesState (position s : openValves s) (position s)
   in if position s `notElem` openValves s && flowRate curValve > 0
        then stateWithOpenValve : nextStates
        else nextStates

allPossibleCombinedMoves :: Tunnels -> CombinedState -> [CombinedState]
allPossibleCombinedMoves _ cs@(CombinedState _ _ []) = [cs]
allPossibleCombinedMoves t (CombinedState openValves hist (p : pr)) =
  let nextStates = allPossibleMoves t (ValvesState openValves p)
   in concatMap recAndCombine nextStates
  where
    recAndCombine (ValvesState o newp) = map (mergeState newp) $ allPossibleCombinedMoves t (CombinedState o hist pr)
    mergeState newp (CombinedState newOpenValves _ newpl) =
      let nextFlow = currentFlow t openValves
          nextPos = newp : newpl
       in CombinedState newOpenValves ((nextPos, nextFlow) : hist) nextPos

filterBestMoves :: Tunnels -> Int -> [CombinedState] -> [CombinedState]
filterBestMoves t m = filterLowFlow . map bestFlow . groupBy ((==) `on` state) . sortOn state
  where
    state (CombinedState o _ p) = (currentFlow t o, p)
    flowEstimation cs@(CombinedState o _ _) = m * currentFlow t o + totalFlow cs
    filterLowFlow s = let best = totalFlow $ bestFlow s in filter ((>= best) . flowEstimation) s

nextBestMoves :: Tunnels -> Int -> [CombinedState] -> [CombinedState]
nextBestMoves t m = filterBestMoves t m . concatMap (allPossibleCombinedMoves t)

runMoves :: CombinedState -> Int -> Tunnels -> [CombinedState]
runMoves i n t = foldl (flip $ nextBestMoves t) [i] [n, n - 1 .. 1]

bestState :: CombinedState -> Int -> Tunnels -> CombinedState
bestState i n = bestFlow . runMoves i n

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
part1 = totalFlow . bestState initialState 30 <$> parseInput
  where
    initialState = CombinedState [] [] ["AA"]

part2 :: Parser Int
part2 = totalFlow . bestState initialState 26 <$> parseInput
  where
    initialState = CombinedState [] [] ["AA", "AA"]

--main :: IO ()
--main = parseAndSolve "inputs/day16" part1 part2
