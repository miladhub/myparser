{-# LANGUAGE OverloadedStrings #-}

module Ex5 where

import Text.Trifecta
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.List as L
import Data.Map.Strict as M
import Data.Text (pack, strip, unpack)
import Control.Monad.State.Lazy

type Year = Integer
type Month = Integer
type Day = Integer
type Hour = Integer
type Minute = Integer
type Activity = String

data LogEntry =
    DateEntry Year Month Day
  | HourMinuteEntry Hour Minute Activity
  | LogComment String
  deriving (Show, Eq)

type When = (Year, Month, Day, Hour, Minute)
type Start = When
type End = When

data Duration = Duration Start End

type Date = (Year, Month, Day)
data ActivitySum =
  ActivitySum (M.Map Activity Integer)
  deriving (Show, Eq)

s :: [LogEntry] -> ActivitySum
s ls = L.foldl f (ActivitySum $ fromList []) ls

f :: ActivitySum -> LogEntry -> ActivitySum
f = undefined

parseLines :: Parser (Date, [LogEntry])
parseLines = do
  (DateEntry yr m d) <- (skipOptional parseComment *> skipOptional newline *> parseDateEntry)
  logs <- many (skipOptional parseComment *> skipOptional newline *> parseHourMinuteEntry)
  many (skipOptional newline)
  return ((yr, m, d), logs)

parseLine :: Parser LogEntry
parseLine =
      parseDateEntry
  <|> parseHourMinuteEntry
  <|> parseComment

parseHourMinuteEntry :: Parser LogEntry
parseHourMinuteEntry = do
  hour <- token decimal
  char ':'
  min <- token decimal
  log <- try ( manyTill anyChar (string "--") )
         <|> many anyChar
  return $ HourMinuteEntry hour min (unpack . strip . pack $ log)

parseDateEntry :: Parser LogEntry
parseDateEntry = do
  token $ string "#"
  year <- token decimal
  char '-'
  month <- token decimal
  char '-'
  day <- decimal
  return $ DateEntry year month day

parseComment :: Parser LogEntry
parseComment = do
  token $ string "--"
  comment <- parseWord
  return $ LogComment comment

parseWord :: Parser String
parseWord = many anyChar

{-
import Data.List
import Text.Trifecta
sequenceA <$> ((fmap . fmap) (parseString parseLine mempty) $ (filter (/= "") . lines) <$> readFile "ex5.log")
l = [LogComment "wheee a comment",DateEntry 2025 2 5,HourMinuteEntry 8 0 "Breakfast",HourMinuteEntry 9 0 "Sanitizing moisture collector",HourMinuteEntry 11 0 "Exercising in high-grav gym",HourMinuteEntry 12 0 "Lunch",HourMinuteEntry 13 0 "Programming",HourMinuteEntry 17 0 "Commuting home in rover",HourMinuteEntry 17 30 "R&R",HourMinuteEntry 19 0 "Dinner",HourMinuteEntry 21 0 "Shower",HourMinuteEntry 21 15 "Read",HourMinuteEntry 22 0 "Sleep",DateEntry 2025 2 7,HourMinuteEntry 8 0 "Breakfast",HourMinuteEntry 9 0 "Bumped head, passed out",HourMinuteEntry 13 36 "Wake up, headache",HourMinuteEntry 13 37 "Go to medbay",HourMinuteEntry 13 40 "Patch self up",HourMinuteEntry 13 45 "Commute home for rest",HourMinuteEntry 14 15 "Read",HourMinuteEntry 21 0 "Dinner",HourMinuteEntry 21 15 "Read",HourMinuteEntry 22 0 "Sleep"]
putStrLn $ intercalate "\n" (show <$> l)
-}

