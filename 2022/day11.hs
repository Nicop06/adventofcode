import Control.Applicative ((<**>))
import Control.Monad
import Data.List
import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

type MonkeyId = Int

type Item = Int

type Operation = Int -> Int

type ThrowTest = Int -> Int

type ThrownItem = (MonkeyId, Item)

data Monkey = Monkey MonkeyId Int [Item] Operation ThrowTest

-- Helpers

condition :: (Int -> Bool) -> Int -> Int -> Int -> Int
condition f vt vf x = if f x then vt else vf

divisibleBy :: Int -> Int -> Bool
divisibleBy x = (== 0) . (`rem` x)

executeRound :: [Monkey] -> [Monkey]
executeRound = id

throwItemsR :: Monkey -> [ThrownItem] -> (Monkey, [ThrownItem])
throwItemsR m@(Monkey _ _ [] _ _) items = (m, items)
throwItemsR (Monkey i n (item : xs) op test) thrownItems = throwItemsR (Monkey i (n + 1) xs op test) ((toMonkey, newItem) : thrownItems)
  where
    newItem = op item
    toMonkey = test newItem

throwItems :: Monkey -> (Monkey, [ThrownItem])
throwItems = flip throwItemsR []

receivedItems :: MonkeyId -> [ThrownItem] -> ([Item], [ThrownItem])
receivedItems monkeyId [] = ([], [])
receivedItems monkeyId ((toMonkey, item) : rs)
  | monkeyId == toMonkey = (item : itemsForMonkey, itemsForOtherMonkeys)
  | otherwise = (itemsForMonkey, (toMonkey, item) : itemsForOtherMonkeys)
  where
    (itemsForMonkey, itemsForOtherMonkeys) = receivedItems monkeyId rs

monkeyWithThrownItems :: Monkey -> [ThrownItem] -> (Monkey, [ThrownItem])
monkeyWithThrownItems (Monkey i n items op test) thrownItems =
  let (extraItems, rs) = receivedItems i thrownItems
   in (Monkey i n (items ++ extraItems) op test, rs)

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
numInspectedItems (Monkey _ n _ _ _) = n

monkeyItems :: Monkey -> [Int]
monkeyItems (Monkey _ _ items _ _) = items

applyOp :: (Int -> Int) -> Monkey -> Monkey
applyOp newOp (Monkey i n items op test) = Monkey i n items (newOp . op) test

levelOfMonkeyBusiness :: Int -> [Monkey] -> Int
levelOfMonkeyBusiness numRounds = product . take 2 . reverse . sort . map numInspectedItems . fst . processRounds numRounds

-- Parser

startMonkey :: Parser MonkeyId
startMonkey = read <$> (string "Monkey " *> many1 digit <* char ':' <* newline)

startingItems :: Parser [Item]
startingItems = map read <$> (string "  Starting items: " *> many1 (many1 digit <* optional (string ", ")) <* newline)

operationParser :: Parser Operation
operationParser = (string "  Operation: new = old " *> operator <* char ' ') <**> (operatorOnNumber <|> operatorOnSelf)

operatorOnNumber :: Parser ((Int -> Int -> Int) -> Operation)
operatorOnNumber = flip ($) . read <$> many1 digit <* newline

operatorOnSelf :: Parser ((Int -> Int -> Int) -> Operation)
operatorOnSelf = join <$ string "old" <* newline

operator :: Parser (Int -> Int -> Int)
operator = ((+) <$ char '+') <|> ((*) <$ char '*')

conditionParser :: Parser (Int -> Bool)
conditionParser = divisibleBy . read <$> (string "  Test: divisible by " *> many1 digit <* newline)

resultIfTrue :: Parser Int
resultIfTrue = read <$> (string "    If true: throw to monkey " *> many1 digit <* newline)

resultIfFalse :: Parser Int
resultIfFalse = read <$> (string "    If false: throw to monkey " *> many1 digit <* newline)

throwTestParser :: Parser ThrowTest
throwTestParser = condition <$> conditionParser <*> resultIfTrue <*> resultIfFalse

monkey :: Parser Monkey
monkey = Monkey <$> startMonkey <*> return 0 <*> startingItems <*> operationParser <*> throwTestParser <* many newline

input :: Parser [Monkey]
input = many1 monkey <* eof

part1 = levelOfMonkeyBusiness 20 . map (applyOp (`div` 3)) <$> input
part2 = levelOfMonkeyBusiness 10000 <$> input

main :: IO ()
main = parseAndSolve "inputs/day11" part1 part2
