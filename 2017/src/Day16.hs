{-# LANGUAGE NumericUnderscores #-}

module Day16
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (elemIndex)
import Data.Map as M (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String

data Instruction
  = Spin Int
  | Exchange Int Int
  | Partner Char Char
  deriving (Show, Eq)

type Program = Char

programs :: [Program]
programs = ['a' .. 'p']

applyInstruction :: [Program] -> Instruction -> [Program]
applyInstruction p (Spin s) = let i = length p - s in drop i p ++ take i p
applyInstruction p (Partner x y) =
  applyInstruction
    p
    (Exchange (fromMaybe 0 $ elemIndex x p) (fromMaybe 0 $ elemIndex y p))
applyInstruction p (Exchange x y)
  | x > y = applyInstruction p (Exchange y x)
  | x == y = p
  | otherwise = take x p ++ [p !! y] ++ take (y - x - 1) (drop (x + 1) p) ++ [p !! x] ++ drop (y + 1) p

repeatInstructions :: Int -> [Instruction] -> [Program]
repeatInstructions numRepeats instructions = go 0 programs M.empty (cycle instructions)
  where
    numInstructions = length instructions
    go :: Int -> [Program] -> Map [Program] Int -> [Instruction] -> [Program]
    go _ _ _ [] = error "No more instruction to run"
    go step p m (i : is) = case M.lookup p m of
      Nothing -> continue
      Just step' ->
        if (step - step') `mod` numInstructions /= 0
          then continue
          else
            let r = (numRepeats * numInstructions) `mod` (step - step')
             in foldr (flip applyInstruction) p (take r $ cycle $ reverse instructions)
      where
        continue = go (step + 1) (applyInstruction p i) (M.insert p step m) is

parseInput :: Parser [Instruction]
parseInput = (parseSpin <|> parseExchange <|> parsePartner) `sepBy1` char ',' <* newline <* eof
  where
    parseInt :: Parser Int
    parseInt = read <$> many1 digit

    parseSpin :: Parser Instruction
    parseSpin = (Spin <$ char 's') <*> parseInt

    parseExchange :: Parser Instruction
    parseExchange = (Exchange <$ char 'x') <*> (parseInt <* char '/') <*> parseInt

    parsePartner :: Parser Instruction
    parsePartner = (Partner <$ char 'p') <*> (alphaNum <* char '/') <*> alphaNum

part1 :: [Instruction] -> IO ()
part1 = putStrLn . foldl applyInstruction programs

part2 :: [Instruction] -> IO ()
part2 = putStrLn . repeatInstructions 1_000_000_000
