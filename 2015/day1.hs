import System.Environment
import Text.Parsec
import Text.Parsec.String

parseChar :: Parser Int
parseChar = (1 <$ char '(') <|> (-1 <$ char ')')

parseInput :: IO (Either ParseError [Int])
parseInput = parseFromFile (many1 parseChar <* eof) "inputs/day1"

part1 :: [Int] -> IO ()
part1 = print . sum

part2 :: [Int] -> IO ()
part2 = print . length . takeWhile (>=0) . scanl (+) 0

main = parseInput >>= either print part2
