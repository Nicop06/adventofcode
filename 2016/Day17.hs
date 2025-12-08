module Day17
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow (first, second)
import Crypto.Hash.MD5 (hash)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Char8 (pack, toStrict, unpack)
import Text.Parsec
import Text.Parsec.String

type Passcode = String

data Direction = U | D | L | R deriving (Show, Eq, Ord)

type Position = (Int, Int)

data PlayerState = PlayerState {playerPosition :: Position, getPath :: [Direction]} deriving (Show, Eq, Ord)

md5Hex :: String -> String
md5Hex = unpack . toStrict . toLazyByteString . byteStringHex . hash . pack

isValidPos :: Position -> Bool
isValidPos (x, y) = x >= 0 && y >= 0 && x < 4 && y < 4

move :: Direction -> Position -> Position
move U = first (subtract 1)
move D = first (+ 1)
move L = second (subtract 1)
move R = second (+ 1)

allowedDirections :: Passcode -> PlayerState -> [Direction]
allowedDirections p (PlayerState pos path) = filterAllowedDir . map fst . filterOpenDoors $ dirAndHashChar
  where
    dirAndHashChar = zip [U, D, L, R] (md5Hex (p ++ concatMap show path))
    filterOpenDoors = filter ((`elem` "bcdef") . snd)
    filterAllowedDir = filter (isValidPos . flip move pos)

nextStates :: Passcode -> PlayerState -> [PlayerState]
nextStates code state@(PlayerState pos path) = map makeNewState $ allowedDirections code state
  where
    makeNewState dir = PlayerState (move dir pos) (path ++ [dir])

allPossibleStates :: Passcode -> [[PlayerState]]
allPossibleStates code = iterate (concatMap (nextStates code) . filter (not . reachVault)) [PlayerState (0, 0) []]

reachVault :: PlayerState -> Bool
reachVault = (== (3, 3)) . playerPosition

allEndStates :: Passcode -> [[PlayerState]]
allEndStates = filter (not . null) . map (filter reachVault) . takeWhile (not . null) . allPossibleStates

parseInput :: Parser String
parseInput = many1 alphaNum <* newline <* eof

part1 :: String -> IO ()
part1 = putStrLn . concatMap show . getPath . head . head . allEndStates

part2 :: String -> IO ()
part2 = print . length . getPath . head . last . allEndStates
