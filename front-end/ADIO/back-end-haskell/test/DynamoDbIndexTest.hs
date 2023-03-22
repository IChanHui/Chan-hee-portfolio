{-# LANGUAGE FlexibleContexts #-}


-- -- All of amazonka APIs use Data.Text.Text by default which is nice
{-# LANGUAGE OverloadedStrings #-}

-- -- Allows record fields to be expanded automatically
{-# LANGUAGE RecordWildCards #-}


module DynamoDbIndexTest where

import DynamoDbTest hiding (DBInfo, getDBInfo, test)

import           Control.Exception.Lens (handling)
import           Control.Lens ((<&>), (^.), (.~), (&), (?~), set)
import           Control.Monad (void, when)
import           Control.Monad.Trans.Resource (MonadUnliftIO, ResourceT)
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
import qualified Data.HashMap.Strict as HashMap (fromList, lookup, singleton, HashMap, toList)
import           Data.Text (Text)
import qualified Data.Text as Text (null, pack, unpack)
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
                    , avNS
                    , avS
                    , avSS
                    , avL
                    , avM
                    , avBOOL
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
                    , qIndexName
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
import           System.IO (stdout)
import Data.Either
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text.Encoding as E
import           Database.Blacktip
import           Database.Blacktip.Types

-- type HostName = ByteString

-- type Port = Int

-- data LoggingState = LoggingEnabled | LoggingDisabled

-- data ServiceType = AWS Region | Local HostName Port

data DBInfo = DBInfo
    { env :: Env
    , service :: Service
    , region :: Region
    , tableName :: Text
    , indexName :: Text
    } 

-- data SendGuide = SendGuide Integer (Text, [Text])
--     deriving (Show, Eq)



getDBInfo :: LoggingState -> ServiceType -> IO DBInfo
getDBInfo loggingState serviceType = do
    env <- getEnv loggingState
    let (service, region) = serviceRegion serviceType
    return $ DBInfo env service region "hackerthon-adio-test" "hackerthon-adio-test-SK-IDs-index"
    where
        -- Standard discovery mechanism for credentials, log to standard output
        getEnv LoggingEnabled = do
            logger <- newLogger Debug stdout
            newEnv Discover <&> set envLogger logger
        -- Standard discovery mechanism for credentials, no logging
        getEnv LoggingDisabled = newEnv Discover

        -- Run against a DynamoDB instance running on AWS in specified region
        serviceRegion (AWS region) = (dynamoDB, region)
        -- Run against a local DynamoDB instance on a given host and port
        serviceRegion (Local hostName port) = (setEndpoint False hostName port dynamoDB, NorthVirginia)


{- withDynamoDB :: (HasEnv r, MonadBaseControl IO m) => -}
-- withDynamoDB :: (HasEnv r, MonadUnliftIO m) => 
--     r
--     -> Service
--     -> Region
--     -> AWST' r (ResourceT m) a
--     -> m a
-- withDynamoDB env service region action =
--     runResourceT . runAWST env . within region $ do
--         reconfigure service action



getContextSK :: DBInfo -> Text -> IO QueryResponse
getContextSK DBInfo{..} sk = withDynamoDB env service region $ do
  result <- send $ query tableName
      & qIndexName .~ Just indexName 
      & qExpressionAttributeValues .~ HashMap.singleton ":v1" (attributeValue & avS ?~ sk)
      & qKeyConditionExpression ?~ "SK = :v1"
  return result

test :: IO Value
test = do
    db <- getDBInfo LoggingDisabled (AWS Seoul)


    context <- getContextSK db "info"
    return $ toJSONList $ context ^. qrsItems
    
    -- prettyPrint . toJSON $ context
    -- print context
    -- test >>= (prettyPrint . toJSONList . (^. qrsItems))




{-
getContext :: DBInfo -> Text -> Text -> Text -> IO (Maybe (HashMap.HashMap Text AttributeValue))
getContext DBInfo{..} id ids sk = withDynamoDB env service region $ do
    result <- send $ getItem tableName
        & giKey .~ key
    return $ do
        context <- HashMap.lookup "context" (result ^. girsItem)
        return (context ^. avM)

    where key = HashMap.fromList
            [ ("IDs", attributeValue & avS .~ Just (ids <> "#" <> id))
            , ("SK", attributeValue & avS .~ Just sk)
            ]
-}


instance ToJSON QueryResponse where
    toJSON = Data.Aeson.toJSON . map HashMap.toList . (^. qrsItems) 