import System.Environment
import RockPaperScissors

main :: IO ()
main = do
  (input : _) <- getArgs
  contents <- readFile input
  print . totalScore . map readRound . lines $ contents

readRound :: String -> Round
readRound roundLine =
  let [elfCode, playerCode] = words roundLine
      Just elfShape = elfShapeFromCode elfCode
      Just playerShape = playerShapeFromCode playerCode
   in Round elfShape playerShape

