module Day4
  ( parseInput,
    part1,
    part2,
  )
where

import Control.Arrow
import Data.List (group, sort, sortOn)
import Data.Map as M (fromListWith, toList)
import Text.Parsec
import Text.Parsec.String

data EventType
  = Sleep
  | WakeUp
  | Guard Int
  deriving (Show, Eq, Ord)

data DateTime = DateTime
  { year :: Int,
    month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int
  }
  deriving (Show, Eq, Ord)

data Event = Event {date :: DateTime, eventType :: EventType} deriving (Show, Eq, Ord)

type GuardShift = (Int, [Int])

guardShifts :: [Event] -> [GuardShift]
guardShifts = toList . fromListWith (++) . tail . go (0, []) Nothing . sort
  where
    go :: GuardShift -> Maybe Int -> [Event] -> [GuardShift]
    go shift lastSleepTime [] = [wakeUp shift 60 lastSleepTime]
    go shift lastSleepTime (Event d st : es) = case st of
      Guard g' -> wakeUp shift 60 lastSleepTime : go (g', []) Nothing es
      Sleep -> go shift (Just $ minute d) es
      WakeUp -> go (wakeUp shift (minute d) lastSleepTime) Nothing es

    wakeUp :: GuardShift -> Int -> Maybe Int -> GuardShift
    wakeUp shift _ Nothing = shift
    wakeUp (g, st) t (Just lastSleepTime) = (g, st ++ [lastSleepTime .. t - 1])

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseDateTime :: Parser DateTime
parseDateTime =
  DateTime
    <$> (parseInt <* char '-')
    <*> (parseInt <* char '-')
    <*> (parseInt <* char ' ')
    <*> (parseInt <* char ':')
    <*> parseInt

parseEventType :: Parser EventType
parseEventType =
  (WakeUp <$ string "wakes up")
    <|> (Sleep <$ string "falls asleep")
    <|> (Guard <$> between (string "Guard #") (string " begins shift") parseInt)

parseEvent :: Parser Event
parseEvent =
  Event
    <$> between (char '[') (string "] ") parseDateTime
    <*> parseEventType

parseInput :: Parser [Event]
parseInput = parseEvent `sepEndBy1` newline <* eof

part1 :: [Event] -> IO ()
part1 events = print (minuteWithMostSleep * g)
  where
    (g, m) = last . sortOn (length . snd) $ guardShifts events
    minuteWithMostSleep =
      head
        . last
        . sortOn length
        . group
        $ sort m

part2 :: [Event] -> IO ()
part2 events = print (g * head m)
  where
    (g, m) =
      last
        . sortOn (length . snd)
        . map (second (last . sortOn length . group . sort))
        . filter (not . null . snd)
        $ guardShifts events
