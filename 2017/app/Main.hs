import Day1
import Day2
import System.Environment
import Text.Parsec.String

runDay :: Int -> IO ()
runDay 1 = parseAndRun 1 Day1.parseInput [Day1.part1, Day1.part2]
runDay 2 = parseAndRun 2 Day2.parseInput [Day2.part1, Day2.part2]
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
