import Data.List
import ParseAndRun

startOfPacket :: Int -> String -> Int
startOfPacket packetWidth stream
  | length stream < packetWidth = 0
  | startsWithMarker stream = packetWidth
  | otherwise = 1 + startOfPacket packetWidth (drop 1 stream)
  where
    startsWithMarker stream = (length . nub $ take packetWidth stream) == packetWidth

part1 :: [String] -> Int
part1 = startOfPacket 4 . head

part2 :: [String] -> Int
part2 = startOfPacket 14 . head

main :: IO ()
main = parseAndRun "inputs/day6" part1 part2
