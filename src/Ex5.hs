{-# LANGUAGE OverloadedStrings #-}

module Ex5 where

import Text.Trifecta
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.List

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

parseLine :: Parser LogEntry
parseLine =
      parseDateEntry
  <|> parseHourMinuteEntry
  <|> parseComment

parseHourMinuteEntry :: Parser LogEntry
parseHourMinuteEntry = do
  hour <- decimal
  _ <- char ':'
  min <- decimal
  _ <- char ' '
  log <- logWords
  skipOptional parseComment
  return $ HourMinuteEntry hour min log

parseDateEntry :: Parser LogEntry
parseDateEntry = do
  _ <- string "# "
  year <- decimal
  _ <- char '-'
  month <- decimal
  _ <- char '-'
  day <- decimal
  skipOptional parseComment
  return $ DateEntry year month day

parseComment :: Parser LogEntry
parseComment = do
  _ <- string "-- "
  comment <- logWords
  return $ LogComment comment

logWords :: Parser String
logWords = (intercalate " " . filter (/= "")) <$> sepBy logWord (char ' ')

logWord :: Parser String
logWord = many alphaNum

{-
log <- readFile "ex5.log"
p = parseString parseLine mempty 
ll = filter (/= "") . lines $ log
fmap p ll
-}

