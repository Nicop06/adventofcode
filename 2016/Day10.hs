module Day10
  ( parseInput,
    part1,
    part2,
  )
where

import Text.Parsec
import Text.Parsec.String
import Data.List (partition, groupBy)

type Value = Int

type Bot = Int

type Output = Int

data Destination = ToBot Bot | ToOutput Output deriving (Show, Eq)

data Input = Intput Value Bot deriving (Show, Eq)

type LowDestination = Destination

type HighDestination = Destination

data Instruction = ValueToBot Value Bot | BotGives Bot LowDestination HighDestination deriving (Show, Eq)

data BotContent = BotContent { getBot :: Bot, getValue :: Value } deriving (Show, Eq)

executeInstruction :: [BotContent] ->  Instruction ->[BotContent]
executeInstruction c (ValueToBot v b) = BotContent b v : c
executeInstruction c (BotGives b l h) =
    let botValues = map getValue . filter isGiver $ c in 
        botContent l (minimum botValues) ++ botContent h (maximum botValues) ++ filter (not . isGiver) c
  where
    isGiver = contentBelongsTo b
    botContent (ToOutput _) _ = []
    botContent (ToBot b') v = [BotContent b' v]

contentBelongsTo :: Bot -> BotContent -> Bool
contentBelongsTo b (BotContent b' _) = b == b'

canExecute :: [BotContent] -> Instruction -> Bool
canExecute _ (ValueToBot _ _) = True
canExecute c (BotGives b _ _) = (== 2) . length . filter (contentBelongsTo b) $ c

nextContent :: [BotContent] -> [Instruction] -> ([BotContent], [Instruction])
nextContent c i = let (toExecute, ri) = partition (canExecute c) i in (executeInstruction c $ head toExecute, tail toExecute ++ ri)

botWithValues :: Value -> Value -> [BotContent] -> [Bot]
botWithValues v1 v2 = map getBot . concat . filter (hasValues . map getValue) . groupBy sameBot
    where sameBot (BotContent b _) (BotContent b' _) = b == b'
          hasValues vals = v1 `elem` vals && v2 `elem` vals

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
part1 = print . head . filter isSameBot . map (botValues . fst) . takeWhile (not . null . snd) . iterate (uncurry nextContent) . ([],)
    where botValues = map getBot . concatMap (filter isValue) . groupBy sameBot
          sameBot (BotContent b _) (BotContent b' _) = b == b'
          isValue b = let v = getValue b in (v == 17 || v == 61)
          isSameBot [b, b'] = b == b'
          isSameBot _ = False

part2 :: [Instruction] -> IO ()
part2 _ = print 0
