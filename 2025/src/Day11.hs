module Day11
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (first, (&&&))
import Data.Map as M (Map, empty, fromList, insert, lookup)
import Text.Parsec
import Text.Parsec.String

newtype DeviceName = DeviceName String deriving (Show, Eq, Ord)

data Device = Device {getName :: DeviceName, getOutputs :: [DeviceName]} deriving (Show, Eq)

type DeviceMap = Map DeviceName Device

getDevice :: DeviceName -> DeviceMap -> Device
getDevice name deviceMap = case M.lookup name deviceMap of
  Just device -> device
  Nothing -> Device name []

type Cache = Map DeviceName Int

numPaths :: DeviceName -> DeviceName -> DeviceMap -> Int
numPaths from to deviceMap = fst $ visit from M.empty
  where
    visit :: DeviceName -> Cache -> (Int, Cache)
    visit d visited
      | d == to = (1, visited)
      | otherwise = case M.lookup d visited of
          Just p -> (p, visited)
          Nothing ->
            let outputs = getOutputs (getDevice d deviceMap)
             in (fst &&& uncurry (M.insert d)) $
                  foldr (\d' (m, v) -> first (+ m) $ visit d' v) (0, visited) outputs

parseDeviceName :: Parser DeviceName
parseDeviceName = DeviceName <$> many1 alphaNum

parseDevice :: Parser Device
parseDevice = Device <$> parseDeviceName <* string ": " <*> parseDeviceName `sepBy1` char ' '

parseInput :: Parser DeviceMap
parseInput = toDeviceMap <$> parseDevice `sepEndBy1` newline <* eof
  where
    toDeviceMap :: [Device] -> DeviceMap
    toDeviceMap = fromList . map (\d -> (getName d, d))

part1 :: DeviceMap -> IO ()
part1 = print . numPaths (DeviceName "you") (DeviceName "out")

part2 :: DeviceMap -> IO ()
part2 m =
  print
    ( numPaths svr dac m * numPaths dac fft m * numPaths fft out m
        + numPaths svr fft m * numPaths fft dac m * numPaths dac out m
    )
  where
    fft = DeviceName "fft"
    dac = DeviceName "dac"
    svr = DeviceName "svr"
    out = DeviceName "out"
