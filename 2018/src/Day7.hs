module Day7
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Char (ord)
import Data.List (nub)
import Data.Maybe (isNothing, mapMaybe)
import Text.Parsec
import Text.Parsec.String

type Task = Char

type TaskDep = (Task, Task)

data Worker = Working Task Int | Done Task deriving (Show)

allTasks :: [TaskDep] -> [Task]
allTasks deps = nub (map fst deps ++ map snd deps)

resolveOrder :: [TaskDep] -> [Task] -> [Task]
resolveOrder _ [] = []
resolveOrder deps tasks = nextTask : resolveOrder remainingDeps remainingTasks
  where
    nextTask = minimum . filter (not . (`elem` map snd deps)) $ tasks
    remainingTasks = filter (/= nextTask) tasks
    remainingDeps = filter ((/= nextTask) . fst) deps

updateWorker :: Worker -> Worker
updateWorker (Done t) = Done t
updateWorker (Working t 0) = Done t
updateWorker (Working t n) = Working t (n - 1)

doneTask :: Worker -> Maybe Task
doneTask (Done t) = Just t
doneTask _ = Nothing

taskWorkedOn :: Worker -> Maybe Task
taskWorkedOn (Working t _) = Just t
taskWorkedOn _ = Nothing

isWorking :: Worker -> Bool
isWorking = isNothing . doneTask

numStepsToComplete :: Int -> Int -> [TaskDep] -> [Task] -> Int
numStepsToComplete cost numWorkers = go []
  where
    makeWorker :: Task -> Worker
    makeWorker t = Working t (cost + ord t - ord 'A')

    go :: [Worker] -> [TaskDep] -> [Task] -> Int
    go _ _ [] = 0
    go workers deps tasks = (if null workers then 0 else 1) + go newWorkers remainingDeps remainingTasks
      where
        updatedWorkers = map updateWorker workers
        doneTasks = mapMaybe doneTask updatedWorkers
        remainingTasks = filter (not . (`elem` doneTasks)) tasks
        remainingDeps = filter (not . (`elem` doneTasks) . fst) deps
        blockedTasks = map snd remainingDeps ++ mapMaybe taskWorkedOn updatedWorkers
        availableTasks = filter (not . (`elem` blockedTasks)) remainingTasks
        newWorkers = take numWorkers $ filter isWorking updatedWorkers ++ map makeWorker availableTasks

parseDependency :: Parser TaskDep
parseDependency =
  (,)
    <$> between (string "Step ") (string " must be finished before step ") anyChar
    <*> (anyChar <* string " can begin.")

parseInput :: Parser [TaskDep]
parseInput = parseDependency `sepEndBy1` newline <* eof

part1 :: [TaskDep] -> IO ()
part1 deps = putStrLn $ resolveOrder deps (allTasks deps)

part2 :: [TaskDep] -> IO ()
part2 deps = print $ numStepsToComplete 60 5 deps (allTasks deps)
