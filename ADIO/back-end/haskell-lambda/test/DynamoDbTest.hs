{-# LANGUAGE FlexibleContexts #-}


-- All of amazonka APIs use Data.Text.Text by default which is nice
{-# LANGUAGE OverloadedStrings #-}

-- Allows record fields to be expanded automatically
{-# LANGUAGE RecordWildCards #-}

module DynamoDbTest where

-- All imports are explicit so we can see exactly where each function comes from
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
import           Data.List (sort)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import qualified Data.Text as Text (null, pack, unpack)
import           Data.Text.Read (decimal)
import           Data.Aeson
import           Data.Scientific
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
import           Control.Concurrent  (forkIO)
import           Text.Read (readMaybe)

import Data.Either
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as TIO
import           Database.Blacktip
import           Database.Blacktip.Types

import DayOfWeekMessageTest


type HostName = ByteString

type Port = Int

data LoggingState = LoggingEnabled | LoggingDisabled

data ServiceType = AWS Region | Local HostName Port

data DBInfo = DBInfo
    { env :: Env
    , service :: Service
    , region :: Region
    , tableName :: Text
    } 

data SendGuide = SendGuide Integer (Text, [Text])
    deriving (Show, Eq)


-- testChannelId =....
-- testThreadId = ..

lsbToText :: LBS.ByteString -> Text
lsbToText = E.decodeUtf8 . LBS.toStrict

jsonToText :: Value -> Text
jsonToText = lsbToText . encodePretty 

prettyPrint :: Value -> IO ()
prettyPrint = TIO.putStrLn . jsonToText 


intToText :: Int -> Text
intToText = Text.pack . show

parseInt :: Text -> Maybe Int
parseInt s = case decimal s of
    Left _ -> Nothing
    Right (result, s') -> if Text.null s' then Just result else Nothing

getDBInfo :: LoggingState -> ServiceType -> IO DBInfo
getDBInfo loggingState serviceType = do
    env <- getEnv loggingState
    let (service, region) = serviceRegion serviceType
    return $ DBInfo env service region "hackerthon-adio-test"
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
withDynamoDB :: (HasEnv r, MonadUnliftIO m) => 
    r
    -> Service
    -> Region
    -> AWST' r (ResourceT m) a
    -> m a
withDynamoDB env service region action =
    runResourceT . runAWST env . within region $ do
        reconfigure service action


-- Gets an item from the DynamoDB table
doGetItem :: DBInfo -> IO (Maybe Int)
doGetItem DBInfo{..} = withDynamoDB env service region $ do
    result <- send $ getItem tableName
        & giKey .~ key
    return $ do
        valueAttr <- HashMap.lookup "val" (result ^. girsItem)
        valueNStr <- valueAttr ^. avN
        parseInt valueNStr
    where key = HashMap.fromList
            [ ("IDs", attributeValue & avS .~ Just "TEST")
            , ("SK", attributeValue & avS .~ Just "data1")
            ]

getContextIDs :: DBInfo -> Text -> Text -> IO QueryResponse
getContextIDs DBInfo{..} ids id = withDynamoDB env service region $ do
  result <- send $ query tableName
      & qExpressionAttributeValues .~ HashMap.singleton ":v1" (attributeValue & avS ?~ (ids <> "#" <> id))
      & qKeyConditionExpression ?~ "IDs = :v1"
  return result


getContext :: DBInfo -> Text -> Text -> Text -> IO (Maybe (HashMap.HashMap Text AttributeValue))
getContext DBInfo{..} ids id sk = withDynamoDB env service region $ do
    result <- send $ getItem tableName
        & giKey .~ key
    return $ do
        context <- HashMap.lookup "context" (result ^. girsItem)
        return (context ^. avM)

    where key = HashMap.fromList
            [ ("IDs", attributeValue & avS .~ Just (ids <> "#" <> id))
            , ("SK", attributeValue & avS .~ Just sk)
            ]

getSK :: HashMap.HashMap Text AttributeValue -> Maybe String
getSK path = do
    sk' <- HashMap.lookup "SK" path
    sk <- Text.unpack <$> sk' ^. avS
    return sk


doQuery :: DBInfo -> IO QueryResponse
doQuery DBInfo{..} = withDynamoDB env service region $ do
  result <- send $ query tableName
      & qExpressionAttributeValues .~ HashMap.singleton ":v1" (attributeValue & avS ?~ "USER1")
      & qKeyConditionExpression ?~ "IDs = :v1"
  return result

-- Puts an item into the DynamoDB table
doPutItem' :: DBInfo -> Text -> Int -> IO ()
doPutItem' DBInfo{..} text value = withDynamoDB env service region $ do
    void $ send $ putItem tableName
        & piItem .~ item
    where item = HashMap.fromList
            [ ("IDs", attributeValue & avS .~ Just text)
            , ("SK", attributeValue & avS .~ Just ("data" <> Text.pack (show value)))
            , ("val", attributeValue & avN .~ Just (intToText value))
            ]

doPutItem :: DBInfo -> Text -> Text -> Text -> Maybe (HashMap.HashMap Text AttributeValue) -> IO ()
doPutItem _ _ _ _ Nothing = putStrLn "No data to put DynamoDb"
doPutItem DBInfo{..} ids id sk (Just value) = withDynamoDB env service region $ do
    void $ send $ putItem tableName
        & piItem .~ item
    where item = HashMap.fromList
            [ ("IDs", attributeValue & avS .~ Just (ids <> "#" <> id))
            , ("SK", attributeValue & avS .~ Just sk)
            , ("context", attributeValue & avM .~ value )
            ]

doPutItem2 :: DBInfo -> Text -> Text -> Maybe (HashMap.HashMap Text AttributeValue) -> IO ()
doPutItem2 _ _ _ Nothing = putStrLn "No data to put DynamoDb"
doPutItem2 DBInfo{..} ids sk (Just value) = withDynamoDB env service region $ do
    void $ send $ putItem tableName
        & piItem .~ item
    where item = HashMap.fromList
            [ ("IDs", attributeValue & avS .~ Just ids)
            , ("SK", attributeValue & avS .~ Just sk)
            , ("context", attributeValue & avM .~ value )
            ]

doDeleteItem :: DBInfo -> Text -> String -> String -> IO ()
-- doDeleteItem _ _ _ _ Nothing = putStrLn "No data to put DynamoDb"
doDeleteItem DBInfo{..} id ids sk = withDynamoDB env service region $ do
    void $ send $ deleteItem tableName
        & diKey .~ item
    where item = HashMap.fromList
            [ ("IDs", attributeValue & avS .~ Just ((Text.pack ids) <> "#" <> id))
            , ("SK", attributeValue & avS .~ Just (Text.pack sk))
            ]

doPutUserInfo :: DBInfo -> Text -> Text -> Maybe (HashMap.HashMap Text AttributeValue) -> IO ()
doPutUserInfo _ _ _ Nothing = putStrLn "No data to put into DynamoDb"
doPutUserInfo DBInfo{..} id sk (Just value) = withDynamoDB env service region $ do
    void $ send $ putItem tableName
        & piItem .~ item
    where item = HashMap.fromList
            [ ("IDs", attributeValue & avS .~ Just ("USER#" <> id))
            , ("SK", attributeValue & avS .~ Just sk)
            , ("context", attributeValue & avM .~ value )
            ]

putUserContextMap :: Text -> Int -> Int -> HashMap.HashMap Text AttributeValue
putUserContextMap userName exp level = HashMap.fromList
    [ ("name", attributeValue & avS .~ Just userName )
    , ("level", attributeValue & avN .~ Just (Text.pack $ show level))
    , ("exp", attributeValue & avN .~ Just (Text.pack $ show exp))
    ]

doUserQuery :: DBInfo -> IO QueryResponse
doUserQuery DBInfo{..} = withDynamoDB env service region $ do
  result <- send $ query tableName
      & qExpressionAttributeValues .~ HashMap.singleton ":v1" (attributeValue & avS ?~ "USER#602853327784640532")
      & qKeyConditionExpression ?~ "IDs = :v1"
  return result

-- doUpdated :: Maybe (HashMap.HashMap Text AttributeValue) -> Maybe (HashMap.HashMap Text AttributeValue)
-- doUpdated path = do
--     case path of
--         Just context -> do -- Maybe 
--             userName <- HashMap.lookup "name" (context)
--             attendance' <- HashMap.lookup "attendance" (context)

--             -- attendance :: [Text]
--             attendance <- Just $ attendance' ^. avSS
--             -- changeTime attendance (Just $ length attendance)
--             updated <- Just $ localToUTC attendance

--             return $ HashMap.fromList [("name",  userName )
--                             , ("attendance", attributeValue & avSS .~ updated)
--                             ]
--     where 
--         toNumberAttr num = attributeValue & avN .~ Just (Text.pack . show $ num)

-- tt = test >>= prettyPrint . toJSON . head
test :: IO ()
test = do
    db <- getDBInfo LoggingDisabled (AWS Seoul)
    -- context <- getContext db "602853327784640532" -- getContext db id
    -- print $ updateContext 100 context           -- updateContext diceResult context

{-     putStrLn "DeleteTable"
 -     doDeleteTableIfExists db
 -
 -     putStrLn "CreateTable"
 -     doCreateTableIfNotExists db -}


    {- putStrLn "UpdateItem"
     - doUpdateItem db -}

    {- putStrLn "GetItem"
     - res <- doGetItem db
     - print res -}

    -- putStrLn "doUserQuery"
    -- res <- doUserQuery db 
    -- print res

    -- localtime -> utctime -> doPutUserInfo
    --     -- 602853327784640532 (Baaaam)@
    --     -- 796164596988641300 (돌뿌)
    --     -- 925267903836741652 (토끼굴22번길)
    --     -- 560090313754935326 (윤지홀리)
    --     -- 998338659902304296 (물결)
    --     -- 687326243703881793 (leechanwoo)@
    -- let id = Text.pack "998338659902304296"
    -- putStrLn "doGetContext"
    -- context <- getContext db id
    -- atd <- doPutUserInfo db id "ATTENDANCE" (doUpdated context)
    -- print atd

    -- let today = toSeconds $ fromGregorian 2022 9 5 23 0 0
    -- userContext <- getContext db (Text.pack "796164596988641300") "USER" "ATTENDANCE"
    -- let updated = updateAttendance' (Text.pack "루돌프 돌뿌") (Text.pack "796164596988641300") today userContext
    -- doPutItem db (Text.pack "796164596988641300") "USER" "ATTENDANCE" updated
    -- putStrLn "doPut compleat"



    -- let threadId = Text.pack "1031114387706085466"
    -- context <- getContext db threadId "THREAD" "PROGRAM"


    -- -- put items
    -- doPutMessages db threadId $ Program { pgName = "22st4", pgChapter = Ch1 }
    -- putStrLn "doPut compleat"
    -- 30677091998139174000094339137537 -> 1week first
    -- 30677092017305341092678563266560 -> 6week last

    -- -- create query items (test server)
    -- doPutMessages db threadId $ Program { pgName = "22st0", pgChapter = Ch1 }
    -- putStrLn "doPut' query compleat"

    -- -- delete items
    -- context <- getContextIDs db threadId "THREAD" 
    -- doDeleteMessages db threadId "THREAD" $ filter findMassges $ context ^. qrsItems
    -- putStrLn "doDelete query compleat"


    -- doDeleteItem db id ids sk
    -- doDeleteItem db threadId "TEST" "00000000"
    -- putStrLn "delete Item"

    -- doPutUserInfo db (Text.pack "0002") "test" dummy
    -- putStrLn "do put user dummy"

    -- context <- getContext db "0000" "USER" "test"
    -- doPutUserInfo db "0000" "test1" context
    -- -- prettyPrint . toJSON $ context
    -- print context
    -- -- putStrLn "go get user info"

    -- context <- getContextIDs db "USER" "0000"
    context <- getContext db "USER" "0001" "info"
    -- prettyPrint . toJSON $ context
    doPutItem db "USER" "0001" "info" $ dummy
    putStrLn "user edit compleat"
    -- print context


    -- userContext <- getContext db (Text.pack "0000") "USER" "ATTENDANCE"
    -- let updated = deleteLastDay userContext
    -- -- return updated
    -- doPutUserInfo db (Text.pack "0000") "ATTENDANCE" updated
    -- putStrLn "delete attendance at last day is compleat"

    -- 
    -- putStrLn "PutItem"
    -- doPutUserInfo db "002" $ doUpdateLevel "testUser" (15+20) 3
    -- context <- getContext db "001"
    -- updateUserInfo <- updateContext 3 context
    -- let updated = updateContext 3 "testUserr" context
    -- doPutUserInfo db "001" updated

-- TODO: remake queries

-- Creates a table in DynamoDB and waits until table is in active state
-- Demonstrates:
-- * Use of runResourceT, runAWST
-- * Use of reconfigure
-- * How to handle exceptions in lenses
-- * Basic use of amazonka-style lenses
-- * How to wait on an asynchronous operation
doCreateTableIfNotExists :: DBInfo -> IO ()
doCreateTableIfNotExists DBInfo{..} = withDynamoDB env service region $ do
    exists <- handling _ResourceInUseException (const (pure True)) $ do
        void $ send $ createTable
            tableName
            (keySchemaElement "counter_name" Hash :| [])
            (provisionedThroughput 5 5)
            & ctAttributeDefinitions .~ [ attributeDefinition "counter_name" S ]
        return False
    when (not exists) (void $ await tableExists (describeTable tableName))

-- Deletes a table in DynamoDB if it exists and waits until table no longer exists
doDeleteTableIfExists :: DBInfo -> IO ()
doDeleteTableIfExists DBInfo{..} = withDynamoDB env service region $ do
    exists <- handling _ResourceNotFoundException (const (pure False)) $ do
        void $ send $ deleteTable tableName
        return True
    when exists (void $ await tableNotExists (describeTable tableName))

-- Updates an item in the DynamoDB table
doUpdateItem :: DBInfo -> IO ()
doUpdateItem DBInfo{..} = withDynamoDB env service region $ do
    void $ send $ updateItem tableName
        & uiKey .~ key
        & uiUpdateExpression .~ Just "ADD counter_value :increment"
        & uiExpressionAttributeValues .~ exprAttrValues
    where
        key = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "my-counter")
            ]
        exprAttrValues = HashMap.fromList
            [ (":increment", attributeValue & avN .~ Just "1" )
            ]


createMessageItem :: String -> Text -> [Text] -> Maybe (HashMap.HashMap Text AttributeValue)
createMessageItem name msg img = return $ HashMap.fromList [ 
    ("name", attributeValue & avS .~ Just (Text.pack name))
    , ("message", attributeValue & avS .~ Just msg)
    , ("images", attributeValue & avSS .~ img)
    ]

-- doPutMessages
doPutMessages :: DBInfo -> Text -> Program Program22st3Chapters -> IO ()
doPutMessages db threadId Program{ pgName = name, pgChapter = ch} -- = undefined 
    | ch == maxBound = do
        seconds <- getUnixMillis
        (Right num) <- generateUniqueId $ defaultConfig { allowableDowntime = seconds }

        doPutItem db threadId "THREAD" (Text.pack $ show num) (createMessageItem (name ++ (show ch)) (msg ch) (img ch))
    | otherwise = do
        seconds <- getUnixMillis
        (Right num) <- generateUniqueId $ defaultConfig { allowableDowntime = seconds }

        doPutItem db threadId "THREAD" (Text.pack $ show num) (createMessageItem (name ++ (show ch)) (msg ch) (img ch))
        doPutMessages db threadId (Program { pgName = name, pgChapter = (succ ch)})


doDeleteMessages :: DBInfo -> Text -> String -> [HashMap.HashMap Text AttributeValue] -> IO ()
doDeleteMessages db threadId ids [] = return ()
doDeleteMessages db threadId ids (ctx:ctxss) = do
    doDeleteItem db threadId ids sk
    doDeleteMessages db threadId ids ctxss
    where
        (Just sk) = getSK ctx
        -- add filter

findMassges :: HashMap.HashMap Text AttributeValue -> Bool
findMassges path 
    | sk == "PROGRAM" = False
    | otherwise = True
    where
        (Just sk) = getSK path


dummy :: Maybe (HashMap.HashMap Text AttributeValue)
dummy = return $ HashMap.fromList [
     ("name", attributeValue & avS .~ Just "HWANG")
    , ("age", attributeValue & avN .~ Just (Text.pack $ show 20))
    , ("gender", attributeValue & avBOOL .~ Just True)
    , ("impression", attributeValue & avBOOL .~ Just True)
    , ("information", attributeValue & avBOOL .~ Just True)
    , ("wallet_address", attributeValue & avS .~ Just "0xa80b5c5fD6C9bFF56D0690044912488ff364fffa")
    ]


doEditUserInfo :: Maybe (HashMap.HashMap Text AttributeValue) -> Maybe (HashMap.HashMap Text AttributeValue)
doEditUserInfo user = do 
    case user of
        Nothing -> Nothing
        Just context -> do
            information' <- HashMap.lookup "information" context
            impression' <- HashMap.lookup "impression" context
            age' <- HashMap.lookup "age" context

            name' <- HashMap.lookup "name" context
            gender' <- HashMap.lookup "gender" context
            wallet_address' <- HashMap.lookup "wallet_address" context

            return $ HashMap.fromList [ ("information", information' )
                            , ("impression", impression')
                            -- , ("wallet_address", wallet_address')
                            , ("gender", gender')
                            , ("age", age')
                            , ("name", name')
                            ]



editProgramInfo' :: Text -> Text -> Maybe (HashMap.HashMap Text AttributeValue) -> Maybe (HashMap.HashMap Text AttributeValue)
editProgramInfo' userName userId path = do
    case path of
        Nothing -> Nothing
        Just context -> do
            members' <- HashMap.lookup "members" (context)
            program_thread_id' <- HashMap.lookup "program_thread_id" (context)
            program_locale' <- HashMap.lookup "program_locale" (context)
            program_timezone' <- HashMap.lookup "program_timezone" (context)
            limits' <- HashMap.lookup "limits" (context)
            program_cron' <- HashMap.lookup "program_cron" (context)
            program_name' <- HashMap.lookup "program_name" (context)
            program_start_date' <- HashMap.lookup "program_start_date" (context)

            return $ HashMap.fromList [ ("operator_name", attributeValue & avS .~ Just userName )
                            , ("operator_id", attributeValue & avS .~ Just userId)
                            , ("members", members')
                            , ("program_name", program_name')
                            , ("program_thread_id", program_thread_id')
                            , ("program_start_date", program_start_date')
                            , ("program_locale", attributeValue & avS .~ Just "Ko")
                            , ("program_timezone", program_timezone')
                            , ("program_cron", program_cron')
                            , ("limits", limits')
                            ]


editProgramInfo :: Text -> Text -> Integer-> Maybe (HashMap.HashMap Text AttributeValue) -> Maybe (HashMap.HashMap Text AttributeValue)
editProgramInfo programName cron date path = do
    case path of
        Nothing -> Nothing
        Just context -> do
            operator_name' <- HashMap.lookup "operator_name" (context)
            operator_id' <- HashMap.lookup "operator_id" (context)
            members' <- HashMap.lookup "members" (context)
            program_thread_id' <- HashMap.lookup "program_thread_id" (context)
            program_locale' <- HashMap.lookup "program_locale" (context)
            program_timezone' <- HashMap.lookup "program_timezone" (context)
            limits' <- HashMap.lookup "limits" (context)

            return $ HashMap.fromList [ ("operator_name", operator_name' )
                            , ("operator_id", operator_id')
                            , ("members", members')
                            , ("program_name", attributeValue & avS .~ Just programName)
                            , ("program_thread_id", program_thread_id')
                            , ("program_start_date", attributeValue & avN .~ Just (Text.pack . show $ date))
                            , ("program_locale", program_locale')
                            , ("program_timezone", program_timezone')
                            , ("program_cron", attributeValue & avS .~ Just cron)
                            , ("limits", limits')
                            ]






-- getOddIdx :: [Int] -> ([Int], [Int])
-- getOddIdx [] = ([], [])
-- getOddIdx (x:xs) = (x:os, es) 
--     where 
--         (os, es) = getEvenIdx xs 



-- getEvenIdx :: [Int] -> ([Int], [Int])
-- getEvenIdx [] = ([], [])
-- getEvenIdx (x:xs) = (os, x:es)
--     where 
--         (os, es) = getOddIdx xs 

