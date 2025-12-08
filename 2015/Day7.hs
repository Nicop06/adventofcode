module Day7
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Applicative ((<**>))
import Data.Bits
import Data.Map.Strict as M (Map, empty, fromList, insert, lookup)
import Data.Maybe (fromJust)
import Text.Parsec
import Text.Parsec.String

data Wire = Wire String | Number Int deriving (Show)

data Connection = AND Wire Wire | OR Wire Wire | NOT Wire | LSHIFT Wire Int | RSHIFT Wire Int | Value Wire deriving (Show)

type Instruction = (String, Connection)

type Circuit = Map String Connection

-- Helpers

processCircuit :: String -> Circuit -> Int
processCircuit wireName c = fst $ processWireR empty (Wire wireName)
  where
    processWireR cache (Number n) = (n, cache)
    processWireR cache (Wire w) =
      case M.lookup w cache of
        Just val -> (val, cache)
        Nothing ->
          let con = fromJust $ M.lookup w c
              (val', cache') = processInstruction con
           in (val', insert w val' cache')
          where
            processInstruction (AND w1 w2) =
              let (val1, cache1) = processWireR cache w1
                  (val2, cache2) = processWireR cache1 w2
               in (val1 .&. val2, cache2)
            processInstruction (OR w1 w2) =
              let (val1, cache1) = processWireR cache w1
                  (val2, cache2) = processWireR cache1 w2
               in (val1 .|. val2, cache2)
            processInstruction (NOT w') = let (val, cache') = processWireR cache w' in (65535 - val, cache')
            processInstruction (LSHIFT w' n) = let (val, cache') = processWireR cache w' in (val `shiftL` n, cache')
            processInstruction (RSHIFT w' n) = let (val, cache') = processWireR cache w' in (val `shiftR` n, cache')
            processInstruction (Value w') = processWireR cache w'

-- Parse

parseNumber :: Parser Int
parseNumber = read <$> many1 digit

parseWireName :: Parser String
parseWireName = many1 lower

parseWire :: Parser Wire
parseWire = (Wire <$> parseWireName) <|> (Number <$> parseNumber)

parseConnection :: Parser Connection
parseConnection = parseNot <|> ((parseWire <* char ' ') <**> (((parseAnd <|> parseOr <|> parseRshift <|> parseLshift) <* char ' ') <|> pure Value))
  where
    parseNot = (NOT <$ string "NOT ") <*> parseWire <* char ' '
    parseAnd = (AND <$ string "AND ") <*> parseWire
    parseOr = (OR <$ string "OR ") <*> parseWire
    parseLshift = (flip LSHIFT <$ string "LSHIFT ") <*> parseNumber
    parseRshift = (flip RSHIFT <$ string "RSHIFT ") <*> parseNumber

parseInstruction :: Parser Instruction
parseInstruction = flip (,) <$> parseConnection <*> (string "-> " *> parseWireName)

parseInput :: Parser Circuit
parseInput = fromList <$> (parseInstruction `sepEndBy1` newline <* eof)

part1 :: Circuit -> IO ()
part1 = print . processCircuit "a"

part2 :: Circuit -> IO ()
part2 c =
  let valA = processCircuit "a" c
   in print $ processCircuit "a" (M.insert "b" (Value (Number valA)) c)
