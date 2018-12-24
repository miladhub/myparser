module Main where

import Lib
import ReadIni

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

