{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Text.Trifecta
import Control.Applicative
import Control.Monad
import Data.Ratio ((%))

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

stop :: Parser a
stop = unexpected "stop"

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

testOneEof :: IO ()
testOneEof = do
  print $ parseString (one >> eof) mempty "1"
  print $ parseString (one >> eof) mempty "12"
  print $ parseString (oneTwo >> eof) mempty "12"
  print $ parseString (oneTwo >> eof) mempty "123"

parseFoo :: String -> Result String
parseFoo = parseString (string "foo") mempty
{-
*Main Text.Trifecta> parseFoo "foo"
Success "foo"
*Main Text.Trifecta> parseFoo "foobar"
Success "foo"
*Main Text.Trifecta> parseFoo "barfoo"
Failure (ErrInfo {_errDoc = (interactive):1:1: error: expected: "foo"
barfoo<EOF> 
^           , _errDeltas = [Columns 0 0]})
*Main Text.Trifecta> 
-}


p123 :: String -> Result Int
p123 =
  parseString (read <$> p123_) mempty
  where
    p123_ = string "123" <|> string "12" <|> string "1"

mystring :: String -> Parser String
mystring s = forM s char

badFraction = "1/0" 
alsoBad = "10" 
shouldWork = "1/2" 
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  num <- decimal
  char '/'
  den <- decimal
  case den of
    0 -> fail "Denominator is zero"
    _ -> return (num % den)

parseFraction' = parseString parseFraction mempty
{-
yourFuncHere :: Parser Integer
yourFuncHere = do
  int <- integer
  eof
  return int
-}
yourFuncHere :: Parser Integer
yourFuncHere = integer <* eof

type NumberOrString =
  Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
      (Left <$> integer)
  <|> (Right <$> some letter)

