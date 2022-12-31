import Data.Set qualified as S
import Text.Parsec
import Text.Parsec.String

type Point = (Int, Int)

data Rectangle = Rectangle {minPoint :: Point, maxPoint :: Point} deriving (Show, Eq)

type Lights = S.Set Point

type LightInstruction = Lights -> Lights

lights :: Rectangle -> Lights
lights (Rectangle (xMin, yMin) (xMax, yMax)) = S.fromList [(x, y) | x <- [xMin .. xMax], y <- [yMin .. yMax]]

turnOff :: Rectangle -> Lights -> Lights
turnOff r = (`S.difference` lights r)

turnOn :: Rectangle -> Lights -> Lights
turnOn r = (`S.union` lights r)

toggle :: Rectangle -> Lights -> Lights
toggle r l = let lightsToToggle = lights r in (lightsToToggle `S.difference` l) `S.union` (l `S.difference` lightsToToggle)

rectSize :: Rectangle -> Int
rectSize (Rectangle (a, b) (c, d)) = (c - a + 1) * (d - b + 1)

part2 :: Int -> IO ()
part2 = print

followInstructions :: [LightInstruction] -> Lights
followInstructions ins = foldr1 (.) (reverse ins) S.empty

-- Parse

parsePoint :: Parser Point
parsePoint = (,) <$> (parseNumber <* char ',') <*> parseNumber
  where
    parseNumber = read <$> many1 digit

parseRectangle :: Parser Rectangle
parseRectangle = Rectangle <$> (parsePoint <* string " through ") <*> parsePoint

parseTurnOn :: Parser LightInstruction
parseTurnOn = turnOn <$> parseRectangle

parseTurnOff :: Parser LightInstruction
parseTurnOff = turnOff <$> parseRectangle

parseToggle :: Parser LightInstruction
parseToggle = toggle <$> parseRectangle

parseInstruction :: Parser LightInstruction
parseInstruction = char 't' *> ((string "oggle " *> parseToggle) <|> (string "urn o" *> ((string "n " *> parseTurnOn) <|> (string "ff " *> parseTurnOff))))

parseInput :: IO (Either ParseError [LightInstruction])
parseInput = parseFromFile (parseInstruction `sepEndBy1` newline <* eof) "inputs/day6"

part1 :: [LightInstruction] -> IO ()
part1 = print . S.size . followInstructions

main = parseInput >>= either print part1
