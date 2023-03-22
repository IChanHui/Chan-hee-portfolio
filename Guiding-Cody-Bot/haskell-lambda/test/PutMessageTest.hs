{-# LANGUAGE FlexibleContexts #-}


-- All of amazonka APIs use Data.Text.Text by default which is nice
{-# LANGUAGE OverloadedStrings #-}

-- Allows record fields to be expanded automatically
{-# LANGUAGE RecordWildCards #-}

module PutMessageTest where

import           Data.Text (Text)
import qualified Data.Text as Text (null, pack, unpack)
import           Control.Monad.Trans.AWS (Region(..))

import DynamoDbTest hiding (test)
import DayOfWeekMessageTest
import Database.Blacktip.Types
import Database.Blacktip


test :: IO ()
test = do
    db <- getDBInfo LoggingDisabled (AWS Seoul)

    let threadId = Text.pack "1027536681034850364"
    
    doPutMessages db threadId $ Program { pgName = "22st0", pgChapter = Ch1 }
    putStrLn "doPut query compleat"

