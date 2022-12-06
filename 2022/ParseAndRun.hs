module ParseAndRun
  ( parseAndRun,
    parseAndSolve,
  )
where

import System.Environment
import Text.Parsec
import Text.Parsec.String

parseAndRun :: Show a => FilePath -> ([String] -> a) -> ([String] -> a) -> IO ()
parseAndRun input part1 part2 = do
  (part : _) <- getArgs
  contents <- readFile input
  case part of
    "part1" -> print . part1 $ lines contents
    "part2" -> print . part2 $ lines contents
    _ -> return ()

parseAndSolve :: Show a => FilePath -> Parser a -> Parser a -> IO ()
parseAndSolve input part1 part2 = do
  (part : _) <- getArgs
  case part of
    "part1" -> solve part1
    "part2" -> solve part2
    _ -> return ()
  where
    solve parser = parseFromFile parser input >>= either print print
