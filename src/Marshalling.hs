{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Marshalling where

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy (ByteString) 
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ
import Data.Scientific (floatingOrInteger)

sectionJson :: ByteString
sectionJson = [r|
  { "section": {"host": "wikipedia.org"},
    "whatisit": {"red": "intoothandclaw"} }
  |]

data TestData = 
  TestData {
    section :: Host
  , what :: Color
  } deriving (Eq, Show)

newtype Host =
  Host String 
  deriving (Eq, Show)


type Annotation = String

data Color =
    Red Annotation
  | Blue Annotation
  | Yellow Annotation 
  deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData <$> v .: "section" 
             <*> v .: "whatisit"
  parseJSON _ =
    fail "Expected an object for TestData"

instance FromJSON Host where 
  parseJSON (Object v) = 
    Host <$> v .: "host"
  parseJSON _ =
    fail "Expected an object for Host"

instance FromJSON Color where 
  parseJSON (Object v) =
        (Red <$> v .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ =
    fail "Expected an object for Color"

{-
mymain :: IO ()
mymain = do
  let d :: Maybe TestData
      d = decode sectionJson
  print d 
-}

data NumberOrString =
    Numba Integer
  | Stringy Text deriving (Eq, Show)  

instance FromJSON NumberOrString where
  parseJSON (Number i) =
    case floatingOrInteger i of 
      (Left _) ->
        fail "Must be integral number" 
      (Right integer) ->
        return $ Numba integer
  parseJSON (String s) = 
    return $ Stringy s
  parseJSON _ =
    fail "NumberOrString must\
         \ be number or string"

-- so it knows what we want to parse
dec :: ByteString
    -> Maybe NumberOrString
dec = decode

eitherDec :: ByteString
          -> Either String NumberOrString
eitherDec = eitherDecode

mymain :: IO ()
mymain = do
  print $ dec "blah"
  print $ eitherDec "blah"
{-
 -
Prelude> :set -XOverloadedStrings
Prelude> decode "{\"blue\": \"123\"}" :: Maybe Color
Just (Blue "123")
Prelude> :set -XQuasiQuotes
Prelude> decode [r|{"red": "123"}|] :: Maybe Color
Just (Red "123")
-}
