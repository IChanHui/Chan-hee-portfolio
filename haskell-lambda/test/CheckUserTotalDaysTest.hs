
{-# LANGUAGE FlexibleContexts #-}

-- All of amazonka APIs use Data.Text.Text by default which is nice
{-# LANGUAGE OverloadedStrings #-}

-- Allows record fields to be expanded automatically
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE QuasiQuotes #-}

module CheckUserTotalDaysTest where

import           Control.Lens ((<&>), (^.), (.~), (&), (?~), set)
import           Control.Monad (void, when)
import           Control.Monad.Trans.AWS (Region(..))
import           Data.Text (Text)
import qualified Data.Text as Text (null, pack, unpack)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup, singleton, HashMap)
import           Network.AWS.DynamoDB
                    ( _ResourceInUseException
                    , _ResourceNotFoundException
                    , KeyType(..)
                    , ScalarAttributeType(..)
                    , attributeDefinition
                    , attributeValue
                    , AttributeValue
                    , avL
                    , avN
                    , avNS
                    , avS
                    , avSS
                    , avM
                    , createTable
                    , ctAttributeDefinitions
                    , deleteTable
                    , describeTable
                    , dynamoDB
                    , getItem
                    , query
                    , giKey
                    , girsItem
                    , keySchemaElement
                    , piItem
                    , provisionedThroughput
                    , putItem
                    , tableExists
                    , tableNotExists
                    , uiExpressionAttributeValues
                    , uiKey
                    , uiUpdateExpression
                    , updateItem
                    , Query
                    , QueryResponse
                    , qKeyConditionExpression
                    , qExpressionAttributeValues
                    , qrsItems
                    )
import Text.RawString.QQ
import NeatInterpolation (text)
import Data.List
import Data.Time.LocalTime
import Text.Read (readMaybe)



import DynamoDb
import MaybeFunction


test :: IO ()
test = do
    db <- getDBInfo LoggingDisabled (AWS Seoul)
    let threadId = Text.pack "1027536681034850364"
    let userId = Text.pack "602853327784640532"

    (Just userContext) <- getContext db userId "USER" (Text.unpack threadId)
    (Just threadContext) <- getContext db threadId "THREAD" "PROGRAM"

    let (Just att) = getAttendance userContext
     
    let (Just psd) = getStartDay threadContext


    let checked = filter (\(i, (Slot x (y,y'))) -> x==1 ) (check psd att)
    let totalCheck = length (groupBy cmpId checked)

    case totalCheck of
        0 -> print "총 0일 출석하셨습니다."
        _ -> do
            let Just (Just (id, (Slot b (sd, ed)))) = headM $ map headM (groupBy cmpId checked)
    
            let totalDays = Text.pack . show $ totalCheck
            let fristDays = Text.pack . show . localDay $ sd

            print (totalDays, fristDays)
    
    -- let totalDays = Text.pack . show $ totalCheck
    -- let fristDays = Text.pack . show . localDay $ sd

    -- return $ [text|
    -- **Baaaam** 님은 ${fristDays} 부터 총 ${totalDays} 일 출석하셨습니다.
    -- |]


getStartDay :: HashMap.HashMap Text AttributeValue -> Maybe Integer
getStartDay threadContext = do 
    program_start_date' <- HashMap.lookup "program_start_date" threadContext
    program_start_date <- Text.unpack <$> program_start_date' ^. avN 
    readMaybe program_start_date




