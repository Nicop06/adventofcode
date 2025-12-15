module Day11
  ( parseInput,
    part1,
    part2,
  )
where

import Data.Map as M (Map, fromList, lookup)
import Data.Set as S (Set, empty, insert)
import Text.Parsec
import Text.Parsec.String

newtype DeviceName = DeviceName String deriving (Show, Eq, Ord)

data Device = Device {getName :: DeviceName, getOutputs :: [DeviceName]} deriving (Show, Eq)

type DeviceMap = Map DeviceName Device

type Path = [DeviceName]

endDevice :: DeviceName
endDevice = DeviceName "out"

getDevice :: DeviceName -> DeviceMap -> Device
getDevice name deviceMap = case M.lookup name deviceMap of
  Just device -> device
  Nothing -> Device name []

allPaths :: DeviceName -> DeviceMap -> [Path]
allPaths fromDevice deviceMap = go S.empty fromDevice
  where
    go :: Set DeviceName -> DeviceName -> [Path]
    go visited d
      | d == endDevice = [[d]]
      | otherwise =
          let visited' = insert d visited
              outputs = getOutputs $ getDevice d deviceMap
           in map (d :) $ concatMap (go visited') outputs

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
part1 = print . length . allPaths (DeviceName "you")

part2 :: DeviceMap -> IO ()
part2 = print . length . filter (\p -> (DeviceName "fft" `elem` p) && (DeviceName "dac" `elem` p)) . allPaths (DeviceName "svr")
