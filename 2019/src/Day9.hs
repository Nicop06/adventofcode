module Day9
  ( parseInput,
    part1,
    part2,
  )
where

import IntCode
import Text.Parsec.String

parseInput :: Parser [Int]
parseInput = parseProgram

part1 :: [Int] -> IO ()
part1 = mapM_ print . flip runProgram [1]

part2 :: [Int] -> IO ()
part2 = mapM_ print . flip runProgram [2]
