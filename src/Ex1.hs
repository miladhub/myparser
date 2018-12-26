{-# LANGUAGE OverloadedStrings #-}

module Ex1 where

import Text.Trifecta
import Control.Applicative
import Control.Monad

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString] 
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

parseSemVer :: Parser SemVer
parseSemVer = do
  maj <- decimal
  _ <- char '.'
  min <- decimal
  _ <- char '.'
  pat <- decimal
  rel <- option [] parseRel
  meta <- option [] parseMeta
  return $ SemVer maj min pat rel meta

parseRel :: Parser Release
parseRel = do
  _ <- char '-'
  sepBy numOrStr (char '.')

parseMeta :: Parser Metadata
parseMeta = do
  _ <- char '+'
  sepBy numOrStr (char '.')

numOrStr :: Parser NumberOrString
numOrStr = do
      NOSI <$> (try (decimal <* eof))
  <|> NOSS <$> some alphaNum
