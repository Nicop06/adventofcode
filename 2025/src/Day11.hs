module Day11
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (second)
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

data CacheState = CacheState
  { pathsWithOutput :: Int,
    pathsWithDAC :: Int,
    pathsWithFFT :: Int,
    pathsWithBoth :: Int
  }
  deriving (Show)

defaultCache :: CacheState
defaultCache =
  CacheState
    { pathsWithOutput = 0,
      pathsWithDAC = 0,
      pathsWithFFT = 0,
      pathsWithBoth = 0
    }

combineCaches :: CacheState -> CacheState -> CacheState
combineCaches c1 c2 =
  CacheState
    { pathsWithOutput = pathsWithOutput c1 + pathsWithOutput c2,
      pathsWithDAC = pathsWithDAC c1 + pathsWithDAC c2,
      pathsWithFFT = pathsWithFFT c1 + pathsWithFFT c2,
      pathsWithBoth = pathsWithBoth c1 + pathsWithBoth c2
    }

visitDevice :: DeviceName -> CacheState -> CacheState
visitDevice d c
  | d == DeviceName "fft" =
      c
        { pathsWithOutput = 0,
          pathsWithDAC = 0,
          pathsWithFFT = pathsWithFFT c + pathsWithOutput c,
          pathsWithBoth = pathsWithBoth c + pathsWithDAC c
        }
  | d == DeviceName "dac" =
      c
        { pathsWithOutput = 0,
          pathsWithFFT = 0,
          pathsWithDAC = pathsWithDAC c + pathsWithOutput c,
          pathsWithBoth = pathsWithBoth c + pathsWithFFT c
        }
  | otherwise = c

getAllPaths :: CacheState -> Int
getAllPaths (CacheState a b c d) = a + b + c + d

type Cache = Map DeviceName CacheState

numPaths :: DeviceName -> DeviceName -> DeviceMap -> CacheState
numPaths from to deviceMap = snd $ go from M.empty
  where
    go :: DeviceName -> Cache -> (Cache, CacheState)
    go d visited
      | d == to = (visited, defaultCache {pathsWithOutput = 1})
      | otherwise = case M.lookup d visited of
          Just p -> (visited, p)
          Nothing ->
            let outputs = getOutputs (getDevice d deviceMap)
                (visited', n) = visit outputs visited
             in (M.insert d n visited', n)

    visit :: [DeviceName] -> Cache -> (Cache, CacheState)
    visit [] visited = (visited, defaultCache)
    visit (d : ds) visited =
      let (visited', n) = go d visited
       in second (combineCaches $ visitDevice d n) $ visit ds visited'

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
part1 = print . getAllPaths . numPaths (DeviceName "you") (DeviceName "out")

part2 :: DeviceMap -> IO ()
part2 = print . pathsWithBoth . numPaths (DeviceName "svr") (DeviceName "out")
