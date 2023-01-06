import ParseAndRun
import Text.Parsec
import Text.Parsec.String

-- Data

parseSnafu :: String -> Int
parseSnafu = parseSnafuR 1 . reverse
  where
    parseSnafuR _ [] = 0
    parseSnafuR p (d : rd) = (p * parseDigit d) + parseSnafuR (p * 5) rd

parseDigit :: Char -> Int
parseDigit '2' = 2
parseDigit '1' = 1
parseDigit '0' = 0
parseDigit '-' = -1
parseDigit '=' = -2
parseDigit d = error ("Invalid digit " ++ [d])

printDigit :: Int -> Char
printDigit 2 = '2'
printDigit 1 = '1'
printDigit 0 = '0'
printDigit (-1) = '-'
printDigit (-2) = '='
printDigit d = error ("Invalid digit " ++ show d)

printSnafu :: Int -> String
printSnafu = reverse . printSnafuR
  where
    printNextDigit 0 = []
    printNextDigit n = printSnafuR n
    printSnafuR n =
      let d = n `rem` 5
       in if d <= 2
            then printDigit d : printNextDigit (n `div` 5)
            else printDigit (d - 5) : printNextDigit ((n + 2) `div` 5)

-- Parser

parseFuelAmount :: Parser Int
parseFuelAmount = parseSnafu <$> many1 (char '2' <|> char '1' <|> char '0' <|> char '-' <|> char '=')

parseInput :: Parser [Int]
parseInput = parseFuelAmount `sepEndBy1` newline <* eof

part1 :: Parser String
part1 = printSnafu . sum <$> parseInput

part2 :: Parser Int
part2 = return 0

main :: IO ()
main = parseAndSolveWithActions putStrLn print "inputs/day25" part1 part2
