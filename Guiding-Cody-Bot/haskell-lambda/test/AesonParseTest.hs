{-# LANGUAGE OverloadedStrings #-}



module AesonParseTest where

import Data.Aeson 
import Data.Text 
import Data.Scientific
import Data.Aeson.Types
import Prelude hiding (lookup)


data ParsedValue = ParsedValue { 
    num :: Maybe Scientific
  , str :: Maybe Text
  , obj :: Maybe Object
} deriving Show


type NumberValue = Scientific 
type StringValue = Text 
type ObjectValue = Object


-- data ParsedValue = ParsedValue (Maybe NumberValue) (Maybe StringValue) (Maybe ObjectValue)

lookup :: Text -> Value -> Maybe Value
lookup key (Object obj) = case parse (.: key) obj of 
                   Error e -> Nothing 
                   Success value -> Just value 
lookup _ _ = Nothing 

example :: Maybe Value
example = Just $ object [ "test" .= Number 1 
                 , "nested" .= object [ "key" .= String "value" ]
                 ]

-- extractValue :: Value -> ParsedValue 
-- extractValue (Number x) = ParsedValue (Just x) Nothing Nothing
-- extractValue (String x) = ParsedValue Nothing (Just x) Nothing
-- extractValue (Object x) = ParsedValue Nothing Nothing (Just x)
-- extractValue _ = ParsedValue Nothing Nothing Nothing


extractValue :: Value -> ParsedValue 
extractValue (Number x) = ParsedValue { num = Just x, str = Nothing, obj = Nothing }
extractValue (String x) = ParsedValue { num = Nothing , str = Just x, obj = Nothing }
extractValue (Object x) = ParsedValue { num = Nothing, str = Nothing, obj = Just x }
extractValue _ = ParsedValue { num = Nothing, str = Nothing, obj = Nothing }



-- test :: Maybe Scientific 
-- test = do 
--     testVal <- lookup "test" example
--     let ParsedValue n s o = extractValue testVal
--     n

    

test :: Maybe Scientific 
test = do
    testVal <- example >>= lookup "test" 
    num $ extractValue testVal
