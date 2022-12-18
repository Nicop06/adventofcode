module ParseAndRun
  ( parseAndRun,
    parseAndSolve,
    parseAndSolveWithActions,
  )
where

import System.Environment
import Text.Parsec.String

parseAndRun :: Show a => FilePath -> ([String] -> a) -> ([String] -> a) -> IO ()
parseAndRun input part1 part2 = do
  (part : _) <- getArgs
  contents <- readFile input
  case part of
    "part1" -> print . part1 $ lines contents
    "part2" -> print . part2 $ lines contents
    _ -> return ()

parseAndSolve :: Show a => Show b => FilePath -> Parser a -> Parser b -> IO ()
parseAndSolve = parseAndSolveWithActions print print

parseAndSolveWithActions :: (a -> IO ()) -> (b -> IO ()) -> FilePath -> Parser a -> Parser b -> IO ()
parseAndSolveWithActions print1 print2 input part1 part2 = do
  (part : _) <- getArgs
  case part of
    "part1" -> solve part1 print1
    "part2" -> solve part2 print2
    _ -> return ()
  where
    solve parser printSol = parseFromFile parser input >>= either print printSol
