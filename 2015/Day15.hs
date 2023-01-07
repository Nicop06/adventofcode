module Day15
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Monad (replicateM)
import Data.List (transpose)
import Text.Parsec
import Text.Parsec.String

-- Data

data Ingredient = Ingredient {getName :: String, getProperties :: [Int], getCalories :: Int} deriving (Show)

-- Helpers

recipeOption :: Int -> [[Int]]
recipeOption n = filter ((== 100) . sum) $ replicateM n [0 .. 100]

totalScore :: [Ingredient] -> [Int] -> Int
totalScore ingredients quantity =
  product $ map (max 0 . sum . zipWith (*) quantity) $ transpose $ map getProperties ingredients

totalCalories :: [Ingredient] -> [Int] -> Int
totalCalories ingredients quantity = sum . zipWith (*) quantity $ map getCalories ingredients

bestScore :: [Ingredient] -> Int
bestScore ingredients = maximum $ map (totalScore ingredients) (recipeOption $ length ingredients)

bestScoreWithCalories :: Int -> [Ingredient] -> Int
bestScoreWithCalories calories ingredients =
  let options = filter ((== calories) . totalCalories ingredients) . recipeOption $ length ingredients
   in maximum $ map (totalScore ingredients) options

-- Parser

properties :: [String]
properties = ["capacity", "durability", "flavor", "texture"]

parseName :: Parser String
parseName = many1 alphaNum

parseNumber :: Parser Int
parseNumber = read <$> many1 (digit <|> char '-')

parseProperty :: String -> Parser Int
parseProperty propName = string (propName ++ " ") *> parseNumber <* string ", "

parseProperties :: Parser [Int]
parseProperties = traverse parseProperty properties

parseCalories :: Parser Int
parseCalories = string "calories " *> parseNumber

parseIngredient :: Parser Ingredient
parseIngredient = Ingredient <$> (parseName <* string ": ") <*> parseProperties <*> parseCalories

parseInput :: Parser [Ingredient]
parseInput = parseIngredient `sepEndBy1` newline <* eof

part1 :: [Ingredient] -> IO ()
part1 = print . bestScore

part2 :: [Ingredient] -> IO ()
part2 = print . bestScoreWithCalories 500
