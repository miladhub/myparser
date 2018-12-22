module Lib where

import Text.Trifecta

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
