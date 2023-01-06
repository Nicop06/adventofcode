import Data.List (group)

input :: String
input = "3113322113"

lookAndSay :: String -> String
lookAndSay = concatMap readGroup . group
  where
    readGroup = (++) <$> show . length <*> take 1

lengthLookAndSay :: Int -> Int
lengthLookAndSay steps = length (iterate lookAndSay input !! steps)

part1 :: IO ()
part1 = print $ lengthLookAndSay 40

part2 :: IO ()
part2 = print $ lengthLookAndSay 50

main :: IO ()
main = sequence_ [part1, part2]
