{-# LANGUAGE OverloadedStrings #-}

module Ex5 where

import Text.Trifecta
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.List
import Data.Text (pack, strip, unpack)
import Data.Time (fromGregorian, NominalDiffTime, localTimeToUTC, utc, LocalTime(..), TimeOfDay(..), diffUTCTime, UTCTime)
import Data.Map (Map, fromList)

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

type LogDateTime = (LogYear, LogMonth, LogDay, LogHour, LogMinute)

data TimedLogEntry =
  TimedLogEntry LogDateTime LogActivity
  deriving (Show, Eq)

activities :: String -> Either String [(LogActivity, NominalDiffTime)]
activities s = measureActivities <$> (>>= attachDates) <$> sortLines s

measureActivities :: [TimedLogEntry] -> [(LogActivity, NominalDiffTime)]
measureActivities (f : s : t) = f `diffEntries` s : measureActivities (s : t)
measureActivities _ = []

diffEntries :: TimedLogEntry -> TimedLogEntry -> (LogActivity, NominalDiffTime)
diffEntries (TimedLogEntry d1 a) (TimedLogEntry d2 _) = (a, diffDates d1 d2)

attachDates :: LogsForDate -> [TimedLogEntry]
attachDates (LogsForDate d ls) = fmap (attachDate d) ls

attachDate :: LogDate -> LogEntry -> TimedLogEntry
attachDate (yr, mo, d) (HourMinuteEntry hr min act) = 
  TimedLogEntry (yr, mo, d, hr, min) act
-- TODO the other cases cannot happen

diffDates :: LogDateTime -> LogDateTime -> NominalDiffTime
diffDates d1 d2 =
  diffUTCTime (logDateTimeToUTC d2) (logDateTimeToUTC d1)

logDateTimeToUTC :: LogDateTime -> UTCTime
logDateTimeToUTC (y, mo, d, h, m) =
  localTimeToUTC utc $ LocalTime
    (fromGregorian y (fromIntegral mo) (fromIntegral d))
    (TimeOfDay (fromIntegral h) (fromIntegral m) 0)

sortLines :: String -> Either String [LogsForDate]
sortLines = (fmap sortLogs) . parseLines . lines 

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

parseLines :: [String] -> Either String [LogEntry]
parseLines = sequenceA . (fmap parseEither) . (filter (/= "")) 

parseEither :: String -> Either String LogEntry
parseEither = resultAsEither . (parseString parseLine mempty)

resultAsEither :: Result a -> Either String a
resultAsEither (Success a) = Right a
resultAsEither (Failure e) = Left (show e)

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
