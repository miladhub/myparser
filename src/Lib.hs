module Lib where

import Text.Trifecta
import Control.Applicative

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
  parseString (read <$> (p3 <|> p2 <|> p1)) mempty

p1 :: Parser String
p1 = string "1"

p2 :: Parser String
p2 = string "12"

p3 :: Parser String
p3 = string "123"
