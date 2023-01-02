import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Text.Parsec
import Text.Parsec.String

type Point = (Int, Int)

data LightInstruction = TurnOn [Point] | TurnOff [Point] | Toggle [Point] deriving (Show, Eq)

-- Parse

parsePoint :: Parser Point
parsePoint = (,) <$> (parseNumber <* char ',') <*> parseNumber
  where
    parseNumber = read <$> many1 digit

parseLights :: Parser [Point]
parseLights = lights <$> (parsePoint <* string " through ") <*> parsePoint
  where
    lights (xMin, yMin) (xMax, yMax) = [(x, y) | x <- [xMin .. xMax], y <- [yMin .. yMax]]

parseTurnOn :: Parser LightInstruction
parseTurnOn = TurnOn <$> parseLights

parseTurnOff :: Parser LightInstruction
parseTurnOff = TurnOff <$> parseLights

parseToggle :: Parser LightInstruction
parseToggle = Toggle <$> parseLights

parseInstruction :: Parser LightInstruction
parseInstruction = char 't' *> ((string "oggle " *> parseToggle) <|> (string "urn o" *> ((string "n " *> parseTurnOn) <|> (string "ff " *> parseTurnOff))))

parseInput :: IO (Either ParseError [LightInstruction])
parseInput = parseFromFile (parseInstruction `sepEndBy1` newline <* eof) "inputs/day6"

part1 :: [LightInstruction] -> IO ()
part1 ins = print . S.size $ foldr followInstruction S.empty (reverse ins)
  where
    followInstruction (TurnOff tl) l = l `S.difference` S.fromList tl
    followInstruction (TurnOn tl) l = l `S.union` S.fromList tl
    followInstruction (Toggle tl) l = let lightsToToggle = S.fromList tl in (lightsToToggle `S.difference` l) `S.union` (l `S.difference` lightsToToggle)

part2 :: [LightInstruction] -> IO ()
part2 ins = print . sum $ foldr followInstruction M.empty (reverse ins)
  where
    followInstruction i = M.filter (> 0) . M.unionWith (+) (M.fromList $ adjustLight i)
    adjustLight (TurnOn l) = (,1) <$> l
    adjustLight (TurnOff l) = (,-1) <$> l
    adjustLight (Toggle l) = (,2) <$> l

main = parseInput >>= either print part2
