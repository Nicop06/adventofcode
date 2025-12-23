import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
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
