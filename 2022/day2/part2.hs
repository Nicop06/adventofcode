import RockPaperScissors
import System.Environment

main :: IO ()
main = do
  (input : _) <- getArgs
  contents <- readFile input
  print . totalScore . map readRound . lines $ contents

readRound :: String -> Round
readRound roundLine =
  let [elfCode, playerCode] = words roundLine
      Just elfShape = elfShapeFromCode elfCode
      Just outcome = outcomeFromCode playerCode
   in Round elfShape (shapeForOutcome elfShape outcome)
