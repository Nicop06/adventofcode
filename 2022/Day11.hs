module Day11(parseInput,part1
,part2) where
import Control.Applicative ((<**>))
import Control.Monad
import Data.List (sort)
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type MonkeyId = Int

type Item = Integer

type DivisibleBy = Item

type Operation = Item -> Item

type ThrownItem = (MonkeyId, Item)

data Monkey = Monkey MonkeyId Int [Item] Operation DivisibleBy MonkeyId MonkeyId

-- Helpers

throwItemsR :: Monkey -> [ThrownItem] -> (Monkey, [ThrownItem])
throwItemsR m@(Monkey _ _ [] _ _ _ _) items = (m, items)
throwItemsR (Monkey i n (item : xs) op by mt mf) thrownItems = throwItemsR (Monkey i (n + 1) xs op by mt mf) ((toMonkey, newItem) : thrownItems)
  where
    newItem = op item
    toMonkey = if newItem `rem` by == 0 then mt else mf

throwItems :: Monkey -> (Monkey, [ThrownItem])
throwItems = flip throwItemsR []

receivedItems :: MonkeyId -> [ThrownItem] -> ([Item], [ThrownItem])
receivedItems _ [] = ([], [])
receivedItems monkeyId ((toMonkey, item) : rs)
  | monkeyId == toMonkey = (item : itemsForMonkey, itemsForOtherMonkeys)
  | otherwise = (itemsForMonkey, (toMonkey, item) : itemsForOtherMonkeys)
  where
    (itemsForMonkey, itemsForOtherMonkeys) = receivedItems monkeyId rs

monkeyWithThrownItems :: Monkey -> [ThrownItem] -> (Monkey, [ThrownItem])
monkeyWithThrownItems (Monkey i n items op by mt mf) thrownItems =
  let (extraItems, rs) = receivedItems i thrownItems
   in (Monkey i n (items ++ extraItems) op by mt mf, rs)

processOneRound :: [Monkey] -> [ThrownItem] -> ([Monkey], [ThrownItem])
processOneRound [] items = ([], items)
processOneRound (m : rs) items =
  let (m1, otherItems) = monkeyWithThrownItems m items
      (m2, thrownItems) = throwItems m1
      (monkeys, newThrownItems) = processOneRound rs (thrownItems ++ otherItems)
   in (m2 : monkeys, newThrownItems)

processRounds :: Int -> [Monkey] -> ([Monkey], [ThrownItem])
processRounds numRounds m =
  let allRounds = replicate numRounds (uncurry processOneRound)
   in foldl (flip ($)) (m, []) allRounds

numInspectedItems :: Monkey -> Int
numInspectedItems (Monkey _ n _ _ _ _ _) = n

monkeyDivisibleBy :: Monkey -> DivisibleBy
monkeyDivisibleBy (Monkey _ _ _ _ by _ _) = by

applyOp :: (Item -> Item) -> Monkey -> Monkey
applyOp newOp (Monkey i n items op by mt mf) = Monkey i n items (newOp . op) by mt mf

levelOfMonkeyBusiness :: Int -> [Monkey] -> Int
levelOfMonkeyBusiness numRounds = product . take 2 . reverse . sort . map numInspectedItems . fst . processRounds numRounds

worryLimiter :: [Monkey] -> [Monkey]
worryLimiter monkeys = map (applyOp (`rem` divByProduct)) monkeys
  where
    divByProduct = product . map monkeyDivisibleBy $ monkeys

-- Parser

startMonkey :: Parser MonkeyId
startMonkey = read <$> (string "Monkey " *> many1 digit <* char ':' <* newline)

startingItems :: Parser [Item]
startingItems = map read <$> (string "  Starting items: " *> many1 (many1 digit <* optional (string ", ")) <* newline)

operationParser :: Parser Operation
operationParser = (string "  Operation: new = old " *> operator <* char ' ') <**> (operatorOnNumber <|> operatorOnSelf)

operatorOnNumber :: Parser ((Item -> Item -> Item) -> Operation)
operatorOnNumber = flip ($) . read <$> many1 digit <* newline

operatorOnSelf :: Parser ((Item -> Item -> Item) -> Operation)
operatorOnSelf = join <$ string "old" <* newline

operator :: Parser (Item -> Item -> Item)
operator = ((+) <$ char '+') <|> ((*) <$ char '*')

conditionParser :: Parser DivisibleBy
conditionParser = read <$> (string "  Test: divisible by " *> many1 digit <* newline)

resultIfTrue :: Parser MonkeyId
resultIfTrue = read <$> (string "    If true: throw to monkey " *> many1 digit <* newline)

resultIfFalse :: Parser MonkeyId
resultIfFalse = read <$> (string "    If false: throw to monkey " *> many1 digit <* newline)

monkey :: Parser Monkey
monkey = Monkey <$> startMonkey <*> return 0 <*> startingItems <*> operationParser <*> conditionParser <*> resultIfTrue <*> resultIfFalse <* many newline

parseInput :: Parser [Monkey]
parseInput = many1 monkey <* eof

part1 :: [Monkey] -> IO()
part1 = print . levelOfMonkeyBusiness 20 . map (applyOp (`div` 3))

part2 :: [Monkey] -> IO()
part2 = print . levelOfMonkeyBusiness 10000 . worryLimiter
