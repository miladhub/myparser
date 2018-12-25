module Main where

import Data.Aeson
import Lib
import ReadIni
import Marshalling
{-
main :: IO ()
main = do
  pNL "stop:" 
  testParse stop 
  pNL "one:" 
  testParse one
  pNL "one':" 
  testParse one' 
  pNL "oneTwo:" 
  testParse oneTwo 
  pNL "oneTwo':" 
  testParse oneTwo'
-}

main :: IO ()
main = mymain
