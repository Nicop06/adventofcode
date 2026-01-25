module Day6
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, pack)
import Text.Parsec
import Text.Parsec.String

type Object = Text

type OrbitMap = M.Map Object [Object]

root :: Object
root = pack "COM"

numOrbits :: OrbitMap -> Int
numOrbits g = go 0 root
  where
    go :: Int -> Object -> Int
    go d o = case M.lookup o g of
      Nothing -> d
      Just children -> d + sum (map (go (d + 1)) children)

pathTo :: Object -> OrbitMap -> [Object]
pathTo t g = head $ catMaybes $ go root
  where
    go :: Object -> [Maybe [Object]]
    go o
      | o == t = [Just []]
      | otherwise = case M.lookup o g of
          Nothing -> []
          Just children -> map ((o :) <$>) $ concatMap go children

removeCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a])
removeCommonPrefix [] l2 = ([], l2)
removeCommonPrefix l1 [] = (l1, [])
removeCommonPrefix (e1 : l1) (e2 : l2)
  | e1 == e2 = removeCommonPrefix l1 l2
  | otherwise = (e1 :) *** (e2 :) $ removeCommonPrefix l1 l2

parseInput :: Parser OrbitMap
parseInput =
  M.fromListWith (++)
    . map (second pure)
    <$> parseOrbit
    `sepEndBy1` newline
    <* eof
  where
    parseObject :: Parser Object
    parseObject = pack <$> many1 alphaNum

    parseOrbit :: Parser (Object, Object)
    parseOrbit = (,) <$> (parseObject <* char ')') <*> parseObject

part1 :: OrbitMap -> IO ()
part1 = print . numOrbits

part2 :: OrbitMap -> IO ()
part2 g =
  print
    . uncurry (+)
    . (length *** length)
    $ removeCommonPrefix pathToYou pathToSan
  where
    pathToYou = pathTo (pack "YOU") g
    pathToSan = pathTo (pack "SAN") g
