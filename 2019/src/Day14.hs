module Day14
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import qualified Data.Map as M
import Data.Text (Text, pack)
import Data.Tuple (swap)
import Text.Parsec
import Text.Parsec.String

type Chemical = Text

type Reactants = M.Map Chemical Int

data Reaction = Reaction
  { _reactants :: [(Int, Chemical)],
    _product :: (Int, Chemical)
  }
  deriving (Show)

fuel :: Chemical
fuel = pack "FUEL"

ore :: Chemical
ore = pack "ORE"

oreNeeded :: Int -> [Reaction] -> Int
oreNeeded fuelToProduce reactions = go (M.fromList [(fuel, fuelToProduce)], M.empty)
  where
    reactionMap :: M.Map Chemical Reaction
    reactionMap = M.fromList $ map (snd . _product &&& id) reactions

    go :: (Reactants, Reactants) -> Int
    go (needed, available) =
      let otherNeeded = M.delete ore needed
          runReaction (chem, amountNeeded) =
            go
              ( first (M.unionWith (+) (M.delete chem otherNeeded)) $
                  react available (chem, amountNeeded)
              )
       in M.findWithDefault 0 ore needed
            + if M.null otherNeeded
              then 0
              else runReaction (M.findMin otherNeeded)

    react :: Reactants -> (Chemical, Int) -> (Reactants, Reactants)
    react available (chem, amountNeeded) =
      let (extra, needed) = reactionCost (reactionMap M.! chem) (amountNeeded - M.findWithDefault 0 chem available)
       in (M.fromList $ map swap needed, M.insert chem extra available)

fuelAmountForOre :: Int -> [Reaction] -> Int
fuelAmountForOre oreAmount reactions = go (1, oreAmount)
  where
    go :: (Int, Int) -> Int
    go (low, high)
      | low == high = low
      | otherwise =
          let mid = low + (high - low) `div` 2
           in case oreNeeded mid reactions `compare` oreAmount of
                EQ -> mid
                GT -> go (low, mid - 1)
                LT -> go (mid + 1, high)

reactionCost :: Reaction -> Int -> (Int, [(Int, Chemical)])
reactionCost (Reaction reactants (quantity, _)) amountNeeded =
  let numReactions = (amountNeeded + quantity - 1) `div` quantity
   in (numReactions * quantity - amountNeeded, map (first (* numReactions)) reactants)

parseInput :: Parser [Reaction]
parseInput = parseReaction `sepEndBy1` newline <* eof
  where
    parseChemical :: Parser Chemical
    parseChemical = pack <$> many1 alphaNum

    parseInt :: Parser Int
    parseInt = read <$> many1 digit

    parseQuantityAndChemical :: Parser (Int, Chemical)
    parseQuantityAndChemical = (,) <$> (parseInt <* char ' ') <*> parseChemical

    parseReaction :: Parser Reaction
    parseReaction =
      Reaction
        <$> (parseQuantityAndChemical `sepEndBy` string ", " <* string " => ")
        <*> parseQuantityAndChemical

part1 :: [Reaction] -> IO ()
part1 = print . oreNeeded 1

part2 :: [Reaction] -> IO ()
part2 = print . fuelAmountForOre 1000000000000
