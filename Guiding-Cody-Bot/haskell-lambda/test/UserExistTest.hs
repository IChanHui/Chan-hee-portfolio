
{-# LANGUAGE FlexibleContexts #-}

-- All of amazonka APIs use Data.Text.Text by default which is nice
{-# LANGUAGE OverloadedStrings #-}

-- Allows record fields to be expanded automatically
{-# LANGUAGE RecordWildCards #-}

module UserExistTest where 

import           Control.Exception.Lens (handling)
import           Control.Lens ((<&>), (^.), (.~), (&), (?~), set)
import           Control.Monad (void, when)
import           Control.Monad.Trans.AWS
                    ( AWST'
                    , Credentials(..)
                    , Env
                    , HasEnv
                    , LogLevel(..)
                    , Region(..)
                    , envLogger
                    , newEnv
                    , newLogger
                    , reconfigure
                    , runAWST
                    , runResourceT
                    , send
                    , setEndpoint
                    , within
                    )
{- import           Control.Monad.Trans.Resource (MonadBaseControl, ResourceT) -}
import           Control.Monad.Trans.Resource (MonadUnliftIO, ResourceT)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup, singleton, HashMap)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import qualified Data.Text as Text (null, pack)
import           Data.Text.Read (decimal)
import           Data.Aeson
import           Network.AWS (Service, await)
import           Network.AWS.DynamoDB
                    ( _ResourceInUseException
                    , _ResourceNotFoundException
                    , KeyType(..)
                    , ScalarAttributeType(..)
                    , attributeDefinition
                    , attributeValue
                    , AttributeValue
                    , avN
                    , avS
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
                    )
import           System.IO (stdout)

import           Network.AWS.DynamoDB.Query (qrsItems)

import DynamoDbTest 


doUserQueryNew :: DBInfo -> Text -> IO QueryResponse -- (HashMap.HashMap Text AttributeValue) --QueryResponse
doUserQueryNew DBInfo{..} userId = withDynamoDB env service region $ do
  result <- send $ query tableName
      & qExpressionAttributeValues .~ HashMap.singleton ":v1" (attributeValue & avS ?~ userId )
      & qKeyConditionExpression ?~ "PK = :v1"
  return result


testExist :: IO () 
testExist = do 
  db <- getDBInfo LoggingDisabled (AWS Seoul)
  res <- doUserQueryNew db "USER#001"
  print res 

testUnknown :: IO () 
testUnknown = do 
  db <- getDBInfo LoggingDisabled (AWS Seoul) 
  res <- doUserQueryNew db "USER#UNKNOWN"
  print res


