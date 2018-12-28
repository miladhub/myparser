{-# LANGUAGE OverloadedStrings #-}

module Ex3 where

import Text.Trifecta
import Control.Monad
import Control.Applicative

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  skipOptional $ string "1-"
  npa <-
        (replicateM 3 $ digit)
    <|> (char '(' *> (replicateM 3 $ digit) <* char ')' <* char ' ')
  skipOptional $ char '-'
  exc <- replicateM 3 $ digit
  skipOptional $ char '-'
  lin <- replicateM 4 $ digit
  return $ PhoneNumber (read npa) (read exc) (read lin)
