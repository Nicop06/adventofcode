import Text.Parsec
import Text.Parsec.String

-- Data

data Token = Backslash | Quote | Hexa String | Character Char deriving (Show, Eq)

type Literal = [Token]

tokenLength :: Token -> Int
tokenLength Backslash = 2
tokenLength Quote = 2
tokenLength (Hexa _) = 4
tokenLength (Character _) = 1

literalLength :: Literal -> Int
literalLength = (+ 2) . sum . map tokenLength

numExtraChar :: Literal -> Int
numExtraChar = (-) <$> literalLength <*> length

-- Parse

parseEscapedString :: Parser [Token]
parseEscapedString = char '"' *> many parseToken <* char '"'
  where
    parseBackslash = Backslash <$ char '\\'
    parseQuote = Quote <$ char '"'
    parseHexa = Hexa <$> (char 'x' *> count 2 alphaNum)
    parseChar = Character <$> alphaNum
    parseToken = (char '\\' *> (parseBackslash <|> parseQuote <|> parseHexa)) <|> parseChar

parseNormalString :: Parser [Token]
parseNormalString = many1 parseToken
  where
    parseBackslash = Backslash <$ char '\\'
    parseQuote = Quote <$ char '"'
    parseChar = Character <$> alphaNum
    parseToken = parseBackslash <|> parseQuote <|> parseChar

parseInput :: Parser Literal -> IO (Either ParseError [Literal])
parseInput parser = parseFromFile (parser `sepEndBy1` newline <* eof) "inputs/day8"

parseAndPrint :: Parser Literal -> IO ()
parseAndPrint parser = parseInput parser >>= either print (print . sum . map numExtraChar)

part1 :: IO ()
part1 = parseAndPrint parseEscapedString

part2 :: IO ()
part2 = parseAndPrint parseNormalString

main :: IO ()
main = sequence_ [part1, part2]
