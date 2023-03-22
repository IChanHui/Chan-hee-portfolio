
{-# LANGUAGE OverloadedStrings #-}

module MessageIOTest where 

import Auth 
import Message 

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import System.IO 
import Control.Monad
import Data.Aeson 

import JsonTest as Json

data EventFrom = FromEventBridge 
               | FromDiscord 
  deriving (Show)


fevent :: IO String
fevent = do 
  handle <- openFile "./resource/message.json" ReadMode 
  contents <- hGetContents handle 
  return contents


scheduledEvent :: IO String
scheduledEvent = do 
  handle <- openFile "./resource/aws-scheduled-event.json" ReadMode 
  contents <- hGetContents handle 
  return contents



test :: IO ()  
test = do 
  event <- scheduledEvent 
  print $ do
    json <- parseJson event 
    data' <- Json.lookup "data" json 
    String name <- Json.lookup "name" data'
    return name 
