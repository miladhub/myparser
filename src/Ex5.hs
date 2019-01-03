{-# LANGUAGE OverloadedStrings #-}

module Ex5 where

import Text.Trifecta
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.List
import Data.Text (pack, strip, unpack)
import Data.Time (fromGregorian, NominalDiffTime, localTimeToUTC, utc, LocalTime(..), TimeOfDay(..), diffUTCTime, UTCTime)

type LogYear = Integer
type LogMonth = Integer
type LogDay = Integer
type LogHour = Integer
type LogMinute = Integer
type LogActivity = String

data LogEntry =
    DateEntry LogYear LogMonth LogDay
  | HourMinuteEntry LogHour LogMinute LogActivity
  | LogComment String
  deriving (Show, Eq)

type LogDate = (LogYear, LogMonth, LogDay)

data LogsForDate =
  LogsForDate LogDate [LogEntry]
  deriving (Show, Eq)

sortLogs :: [LogEntry] -> [LogsForDate]
sortLogs =
  foldl sortLog []
  where
    sortLog ls (DateEntry year month day) = ls ++ [LogsForDate (year, month, day) []]
    sortLog [] _ = []
    sortLog ls (LogComment _) = ls
    sortLog ls e@(HourMinuteEntry _ _ _) =
      let rev = reverse ls
          (LogsForDate (year, month, day) es) = head rev
          new = LogsForDate (year, month, day) $ es ++ [e]
      in reverse $ new : (tail rev)

type LogDateTime = (LogYear, LogMonth, LogDay, LogHour, LogMinute)

data TimedLogEntry =
  TimedLogEntry LogDateTime LogActivity
  deriving (Show, Eq)

diffDates :: LogDateTime -> LogDateTime -> NominalDiffTime
diffDates d1 d2 =
  diffUTCTime (logDateTimeToUTC d2) (logDateTimeToUTC d1)

logDateTimeToUTC :: LogDateTime -> UTCTime
logDateTimeToUTC (y, mo, d, h, m) =
  localTimeToUTC utc $ LocalTime
    (fromGregorian y (fromIntegral mo) (fromIntegral d))
    (TimeOfDay (fromIntegral h) (fromIntegral m) 0)

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
l = sequenceA <$> ((fmap . fmap) (parseString parseLine mempty) $ (filter (/= "") . lines) <$> readFile "ex5.log")
s = (fmap . fmap) sortLogs l
-}

