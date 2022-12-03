module ParseAndRun
(
    parseAndRun
)
where

import System.Environment

parseAndRun :: Show a => String -> ([String] -> a) -> ([String] -> a) -> IO ()
parseAndRun input part1 part2 = do
  (part : _) <- getArgs
  contents <- readFile input
  case part of
    "part1" -> print . part1 $ lines contents
    "part2" -> print . part2 $ lines contents
    _ -> return ()
