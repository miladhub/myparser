{-# LANGUAGE OverloadedStrings #-}

module Ex1 where

import Text.Trifecta
import Control.Applicative
import Control.Monad
import Data.List

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

instance Ord SemVer where
  compare (SemVer maj min pat rel _) (SemVer maj' min' pat' rel' _) =
    let zipped = zip [maj, min, pat] [maj', min', pat']
        majMinPatch = mconcat $ fmap (\(f, s) -> compare f s) $ zipped
        relCompared = compare rel rel'
        relLengthCompared = compare (length rel') (length rel)
    in
      if majMinPatch == EQ then
        if length rel == 0 then
          GT
        else
          if length rel' == 0 then
            LT
          else
            if relCompared == EQ then
              relLengthCompared
            else relCompared
      else majMinPatch

instance Ord NumberOrString where
  compare (NOSI _) (NOSS _) = LT
  compare (NOSI i) (NOSI i') = compare i i'
  compare (NOSS s) (NOSS s') = compare s s'

psv :: String -> Result SemVer
psv = parseString parseSemVer mempty

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
