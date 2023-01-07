module Day8
  ( parseInput,
    part1,
    part2,
  )
where

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

parseContent :: Parser Literal -> String -> Either ParseError [Literal]
parseContent parser = parse (parser `sepEndBy1` newline <* eof) ""

parseAndPrint :: Parser Literal -> String -> IO ()
parseAndPrint parser content = either print (print . sum . map numExtraChar) $ parseContent parser content

parseInput :: Parser String
parseInput = many1 anyChar

part1 :: String -> IO ()
part1 = parseAndPrint parseEscapedString

part2 :: String -> IO ()
part2 = parseAndPrint parseNormalString
