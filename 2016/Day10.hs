module Day10
  ( parseInput,
    part1,
    part2,
  )
where

import Data.List (partition)
import Text.Parsec
import Text.Parsec.String

type Value = Int

type Bot = Int

type Output = Int

data Destination = ToBot Bot | ToOutput Output deriving (Show, Eq)

data Input = Intput Value Bot deriving (Show, Eq)

type LowDestination = Destination

type HighDestination = Destination

data Instruction = ValueToBot Value Bot | BotGives Bot LowDestination HighDestination deriving (Show, Eq)

data Content = BotContent Bot Value | OutputContent Output Value deriving (Show, Eq)

executeInstruction :: [Content] -> Instruction -> [Content]
executeInstruction c (ValueToBot v b) = BotContent b v : c
executeInstruction c (BotGives b l h) =
  let botValues = map getContentValue . filter isGiver $ c
   in botContent l (minimum botValues) : botContent h (maximum botValues) : filter (not . isGiver) c
  where
    isGiver = contentBelongsTo b
    botContent (ToOutput o) v = OutputContent o v
    botContent (ToBot b') v = BotContent b' v

getContentValue :: Content -> Value
getContentValue (BotContent _ v) = v
getContentValue (OutputContent _ v) = v

contentBelongsTo :: Bot -> Content -> Bool
contentBelongsTo b (BotContent b' _) = b == b'
contentBelongsTo _ _ = False

canExecute :: [Content] -> Instruction -> Bool
canExecute _ (ValueToBot _ _) = True
canExecute c (BotGives b _ _) = (== 2) . length . filter (contentBelongsTo b) $ c

nextContent :: [Content] -> [Instruction] -> ([Content], [Instruction])
nextContent c i = let (toExecute, ri) = partition (canExecute c) i in (executeInstruction c $ head toExecute, tail toExecute ++ ri)

iterateInstructions :: [Instruction] -> [[Content]]
iterateInstructions = map fst . takeUntil (null . snd) . iterate (uncurry nextContent) . ([],)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x : rs) = if f x then [x] else x : takeUntil f rs

botWithValues :: Value -> Value -> [[Content]] -> Bot
botWithValues v1 v2 = getBot . head . head . filter isSameBot . map (filter isValue)
  where
    isSameBot [BotContent b _, BotContent b' _] = b == b'
    isSameBot _ = False
    isValue b = let v = getContentValue b in (v == v1 || v == v2)
    getBot (BotContent b _) = b
    getBot _ = error $ "Could not find the bot responsible for " ++ show v1 ++ " and " ++ show v2

parseInput :: Parser [Instruction]
parseInput = parseInstruction `sepEndBy1` newline <* eof

parseInstruction :: Parser Instruction
parseInstruction = parseValueToBot <|> parseBotGives

parseValueToBot :: Parser Instruction
parseValueToBot = ValueToBot <$> (string "value " *> parseNum) <*> (string " goes to bot " *> parseNum)

parseBotGives :: Parser Instruction
parseBotGives = BotGives <$> (string "bot " *> parseNum) <*> (string " gives low to " *> parseDestination) <*> (string " and high to " *> parseDestination)

parseDestination :: Parser Destination
parseDestination = parseBotDest <|> parseOutputDest
  where
    parseBotDest = ToBot <$> (string "bot " *> parseNum)
    parseOutputDest = ToOutput <$> (string "output " *> parseNum)

parseNum :: Parser Int
parseNum = read <$> many1 digit

part1 :: [Instruction] -> IO ()
part1 = print . botWithValues 17 61 . iterateInstructions

part2 :: [Instruction] -> IO ()
part2 = print . product . map getContentValue . filter isExpectedOutput . last . iterateInstructions
  where
    getOutput (OutputContent o _) = Just o
    getOutput _ = Nothing
    isExpectedOutput = (`elem` (Just <$> [0, 1, 2])) . getOutput
