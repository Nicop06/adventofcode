import Day1 qualified as D1
import Day2 qualified as D2
import Day3 qualified as D3
import Day4 qualified as D4
import Day5 qualified as D5
import Day6 qualified as D6
import Day7 qualified as D7
import Day8 qualified as D8
import Day9 qualified as D9
import Day10 qualified as D10
import Day11 qualified as D11
import Day12 qualified as D12
import Day13 qualified as D13
import Day14 qualified as D14
import Day15 qualified as D15
import Day16 qualified as D16
import Day17 qualified as D17
--import Day18 qualified as D18
--import Day19 qualified as D19
--import Day20 qualified as D20
--import Day21 qualified as D21
--import Day22 qualified as D22
--import Day23 qualified as D23
--import Day24 qualified as D24
--import Day25 qualified as D25
import System.Environment
import Text.Parsec.String

runDay :: Int -> IO ()
runDay 1 = parseAndRun 1 D1.parseInput [D1.part1, D1.part2]
runDay 2 = parseAndRun 2 D2.parseInput [D2.part1, D2.part2]
runDay 3 = parseAndRun 3 D3.parseInput [D3.part1, D3.part2]
runDay 4 = parseAndRun 4 D4.parseInput [D4.part1, D4.part2]
runDay 5 = parseAndRun 5 D5.parseInput [D5.part1, D5.part2]
runDay 6 = parseAndRun 6 D6.parseInput [D6.part1, D6.part2]
runDay 7 = parseAndRun 7 D7.parseInput [D7.part1, D7.part2]
runDay 8 = parseAndRun 8 D8.parseInput [D8.part1, D8.part2]
runDay 9 = parseAndRun 9 D9.parseInput [D9.part1, D9.part2]
runDay 10 = parseAndRun 10 D10.parseInput [D10.part1, D10.part2]
runDay 11 = parseAndRun 11 D11.parseInput [D11.part1, D11.part2]
runDay 12 = parseAndRun 12 D12.parseInput [D12.part1, D12.part2]
runDay 13 = parseAndRun 13 D13.parseInput [D13.part1, D13.part2]
runDay 14 = parseAndRun 14 D14.parseInput [D14.part1, D14.part2]
runDay 15 = parseAndRun 15 D15.parseInput [D15.part1, D15.part2]
runDay 16 = parseAndRun 16 D16.parseInput [D16.part1, D16.part2]
runDay 17 = parseAndRun 17 D17.parseInput [D17.part1, D17.part2]
--runDay 18 = parseAndRun 18 D18.parseInput [D18.part1, D18.part2]
--runDay 19 = parseAndRun 19 D19.parseInput [D19.part1, D19.part2]
--runDay 20 = parseAndRun 20 D20.parseInput [D20.part1, D20.part2]
--runDay 21 = parseAndRun 21 D21.parseInput [D21.part1, D21.part2]
--runDay 22 = parseAndRun 22 D22.parseInput [D22.part1, D22.part2]
--runDay 23 = parseAndRun 23 D23.parseInput [D23.part1, D23.part2]
--runDay 24 = parseAndRun 24 D24.parseInput [D24.part1, D24.part2]
--runDay 25 = parseAndRun 25 D25.parseInput [D25.part1, D25.part2]
runDay _ = return ()

parseAndRun :: Int -> Parser a -> [a -> IO ()] -> IO ()
parseAndRun day parser solvers =
  let file = "inputs/day" ++ show day
   in do
        putStrLn $ "=== Day " ++ show day ++ " ==="
        parseFromFile parser file >>= either print (sequence_ . sequenceA solvers)

main :: IO ()
main = do
  args <- getArgs
  mapM_ (runDay . read) args
