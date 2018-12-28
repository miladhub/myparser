{-# LANGUAGE OverloadedStrings #-}

module Ex2 where

import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['1'..'9']

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
  neg <- optional (char '-')
  case neg of
    Nothing -> base10Integer
    _       -> (* (-1)) <$> base10Integer
