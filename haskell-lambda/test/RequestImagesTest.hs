{-# LANGUAGE FlexibleContexts #-}


-- All of amazonka APIs use Data.Text.Text by default which is nice
{-# LANGUAGE OverloadedStrings #-}

-- Allows record fields to be expanded automatically
{-# LANGUAGE RecordWildCards #-}

module RequestImagesTest where

import qualified Data.Text as T 
import           Control.Lens ((<&>), (^.), (.~), (&), (?~), set)
import           Control.Monad.Trans.AWS ( Region(..))
import           Data.Scientific 
import           Data.Aeson
import           Network.AWS.DynamoDB
                    ( _ResourceInUseException
                    , _ResourceNotFoundException
                    , KeyType(..)
                    , ScalarAttributeType(..)
                    , attributeDefinition
                    , attributeValue
                    , AttributeValue
                    , avN
                    , avNS
                    , avS
                    , avSS
                    , avL
                    , avM
                    , createTable
                    , ctAttributeDefinitions
                    , deleteTable
                    , deleteItem
                    , diKey
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
                    , qrsCount
                    , qrsLastEvaluatedKey
                    , qrsResponseStatus
                    , qrsItems
                    )


import MaybeFunctionTest
import Action
import DynamoDb
import Message

getNumber :: Value -> Maybe Scientific
getNumber (Number x) = Just x
getNumber _ = Nothing


test :: IO ()
test = do
    let (Just today) = getNumber (Number 1665906043463)
    let threadId = T.pack "1015987723573657730"
    let channelId = T.pack "1003574118861504553"

    db <- getDBInfo LoggingDisabled (AWS Seoul)

    context <- getContextPK db threadId "THREAD"

    let pairOfMsgImgUrl = do 
        todayMsg <- dayOfMessage today (context ^. qrsItems)
        createWeekMessage todayMsg 

    case pairOfMsgImgUrl of 
        Nothing -> print "can't find today of week message"
        Just (msg, images) -> do
            let requestMsg = ImageMessage channelId msg images
            print "test images log ---"
            print requestMsg
            print "--- ---"

            request requestMsg
            print "== end =="

        

























