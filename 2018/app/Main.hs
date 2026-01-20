import Day1
import Day10
-- import Day23
-- import Day24
-- import Day25
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day2
import Day20
import Day21
import Day22
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import System.Environment
import Text.Parsec.String

runDay :: Int -> IO ()
runDay 1 = parseAndRun 1 Day1.parseInput [Day1.part1, Day1.part2]
runDay 2 = parseAndRun 2 Day2.parseInput [Day2.part1, Day2.part2]
runDay 3 = parseAndRun 3 Day3.parseInput [Day3.part1, Day3.part2]
runDay 4 = parseAndRun 4 Day4.parseInput [Day4.part1, Day4.part2]
runDay 5 = parseAndRun 5 Day5.parseInput [Day5.part1, Day5.part2]
runDay 6 = parseAndRun 6 Day6.parseInput [Day6.part1, Day6.part2]
runDay 7 = parseAndRun 7 Day7.parseInput [Day7.part1, Day7.part2]
runDay 8 = parseAndRun 8 Day8.parseInput [Day8.part1, Day8.part2]
runDay 9 = parseAndRun 9 Day9.parseInput [Day9.part1, Day9.part2]
runDay 10 = parseAndRun 10 Day10.parseInput [Day10.part1, Day10.part2]
runDay 11 = parseAndRun 11 Day11.parseInput [Day11.part1, Day11.part2]
runDay 12 = parseAndRun 12 Day12.parseInput [Day12.part1, Day12.part2]
runDay 13 = parseAndRun 13 Day13.parseInput [Day13.part1, Day13.part2]
runDay 14 = parseAndRun 14 Day14.parseInput [Day14.part1, Day14.part2]
runDay 15 = parseAndRun 15 Day15.parseInput [Day15.part1, Day15.part2]
runDay 16 = parseAndRun 16 Day16.parseInput [Day16.part1, Day16.part2]
runDay 17 = parseAndRun 17 Day17.parseInput [Day17.part1, Day17.part2]
runDay 18 = parseAndRun 18 Day18.parseInput [Day18.part1, Day18.part2]
runDay 19 = parseAndRun 19 Day19.parseInput [Day19.part1, Day19.part2]
runDay 20 = parseAndRun 20 Day20.parseInput [Day20.part1, Day20.part2]
runDay 21 = parseAndRun 21 Day21.parseInput [Day21.part1, Day21.part2]
runDay 22 = parseAndRun 22 Day22.parseInput [Day22.part1, Day22.part2]
-- runDay 23 = parseAndRun 23 Day23.parseInput [Day23.part1, Day23.part2]
-- runDay 24 = parseAndRun 24 Day24.parseInput [Day24.part1, Day24.part2]
-- runDay 25 = parseAndRun 25 Day25.parseInput [Day25.part1, Day25.part2]
runDay _ = return ()

parseAndRun :: Int -> Parser a -> [a -> IO ()] -> IO ()
parseAndRun day parser solvers = runDayX day (parseAndSolve parser solvers)

runDayX :: Int -> (FilePath -> IO ()) -> IO ()
runDayX day solver =
  let file = "inputs/day" ++ show day
   in do
        putStrLn $ "=== Day " ++ show day ++ " ==="
        solver file

parseAndSolve :: Parser a -> [a -> IO ()] -> FilePath -> IO ()
parseAndSolve parser solvers file =
  parseFromFile parser file >>= either print (sequence_ . sequenceA solvers)

main :: IO ()
main = do
  args <- getArgs
  mapM_ (runDay . read) args
