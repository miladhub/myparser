{-# LANGUAGE OverloadedStrings #-}

module Ex5 where

import Text.Trifecta
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.List

data LogEntry =
    DateEntry Integer Integer Integer
  | HourMinuteEntry Integer Integer String
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
  log <- (intercalate " " . filter (/= "")) <$> sepBy word (char ' ')
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
  comment <- (intercalate " " . filter (/= "")) <$> sepBy word (char ' ')
  return $ LogComment comment

word :: Parser String
word = many alphaNum

{-
log <- readFile "ex5.log"
p = parseString parseLine mempty 
ll = filter (/= "") . lines $ log
fmap p ll
-}

