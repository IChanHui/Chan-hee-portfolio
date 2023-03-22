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
import           Data.Time hiding (fromGregorian)
import           Data.Time.Calendar hiding (fromGregorian)
import           Data.DateTime (fromGregorian, toSeconds, fromSeconds)
import           Data.Time.Calendar.WeekDate as CalWee 
import           Text.Read (readMaybe)
import           Database.Blacktip
import           Database.Blacktip.Types
import           Filesystem.Path

import System.Cron hiding (dayOfWeek)
import System.Cron.Types hiding (dayOfWeek)
import System.Cron.Schedule
import System.Cron.Parser
import System.Cron.Internal.Schedule
import Data.Either
import Data.Time.Clock

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as TIO
import MaybeFunctionTest
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
    return $ DBInfo env service region "discord-test1"
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
            [ ("PK", attributeValue & avS .~ Just "TEST")
            , ("SK", attributeValue & avS .~ Just "data1")
            ]


getLevel :: DBInfo -> Text -> IO (Maybe Int)
getLevel DBInfo{..} id = withDynamoDB env service region $ do
    result <- send $ getItem tableName
        & giKey .~ key
    return $ do
        context <- HashMap.lookup "context" (result ^. girsItem)
        levelVal <- HashMap.lookup "level" (context ^. avM)
        level <- levelVal ^. avN
        expVal <- HashMap.lookup "exp" (context ^. avM)
        exp <- expVal ^. avN
        parseInt level

    where key = HashMap.fromList
            [ ("PK", attributeValue & avS .~ Just ("USER#" <> id))
            , ("SK", attributeValue & avS .~ Just "GAME")
            ]


getContextPK :: DBInfo -> Text -> String -> IO QueryResponse
getContextPK DBInfo{..} id pk = withDynamoDB env service region $ do
  result <- send $ query tableName
      & qExpressionAttributeValues .~ HashMap.singleton ":v1" (attributeValue & avS ?~ ((Text.pack pk) <> "#" <> id))
      & qKeyConditionExpression ?~ "PK = :v1"
  return result


getContext :: DBInfo -> Text -> String -> String -> IO (Maybe (HashMap.HashMap Text AttributeValue))
getContext DBInfo{..} id pk sk = withDynamoDB env service region $ do
    result <- send $ getItem tableName
        & giKey .~ key
    return $ do
        context <- HashMap.lookup "context" (result ^. girsItem)
        return (context ^. avM)

    where key = HashMap.fromList
            [ ("PK", attributeValue & avS .~ Just ((Text.pack pk) <> "#" <> id))
            , ("SK", attributeValue & avS .~ Just (Text.pack sk))
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
      & qKeyConditionExpression ?~ "PK = :v1"
  return result

-- Puts an item into the DynamoDB table
doPutItem' :: DBInfo -> Text -> Int -> IO ()
doPutItem' DBInfo{..} text value = withDynamoDB env service region $ do
    void $ send $ putItem tableName
        & piItem .~ item
    where item = HashMap.fromList
            [ ("PK", attributeValue & avS .~ Just text)
            , ("SK", attributeValue & avS .~ Just ("data" <> Text.pack (show value)))
            , ("val", attributeValue & avN .~ Just (intToText value))
            ]

doPutItem :: DBInfo -> Text -> String -> String -> Maybe (HashMap.HashMap Text AttributeValue) -> IO ()
doPutItem _ _ _ _ Nothing = putStrLn "No data to put DynamoDb"
doPutItem DBInfo{..} id pk sk (Just value) = withDynamoDB env service region $ do
    void $ send $ putItem tableName
        & piItem .~ item
    where item = HashMap.fromList
            [ ("PK", attributeValue & avS .~ Just ((Text.pack pk) <> "#" <> id))
            , ("SK", attributeValue & avS .~ Just (Text.pack sk))
            , ("context", attributeValue & avM .~ value )
            ]

doDeleteItem :: DBInfo -> Text -> String -> String -> IO ()
-- doDeleteItem _ _ _ _ Nothing = putStrLn "No data to put DynamoDb"
doDeleteItem DBInfo{..} id pk sk = withDynamoDB env service region $ do
    void $ send $ deleteItem tableName
        & diKey .~ item
    where item = HashMap.fromList
            [ ("PK", attributeValue & avS .~ Just ((Text.pack pk) <> "#" <> id))
            , ("SK", attributeValue & avS .~ Just (Text.pack sk))
            ]

doPutUserInfo :: DBInfo -> Text -> String -> Maybe (HashMap.HashMap Text AttributeValue) -> IO ()
doPutUserInfo _ _ _ Nothing = putStrLn "No data to put into DynamoDb"
doPutUserInfo DBInfo{..} id sk (Just value) = withDynamoDB env service region $ do
    void $ send $ putItem tableName
        & piItem .~ item
    where item = HashMap.fromList
            [ ("PK", attributeValue & avS .~ Just ("USER#" <> id))
            , ("SK", attributeValue & avS .~ Just (Text.pack sk))
            , ("context", attributeValue & avM .~ value )
            ]
-- ATTENDANCE

-- putUserContext :: Text -> Value 
-- putUserContext userName = object [ "fishing" .= object [
--                          "name" .= String userName,
--                          "level" .= String "1",
--                          "exp" .= String "0"]
--                          ]

putUserContextMap :: Text -> Int -> Int -> HashMap.HashMap Text AttributeValue
putUserContextMap userName exp level = HashMap.fromList
    [ ("name", attributeValue & avS .~ Just userName )
    , ("level", attributeValue & avN .~ Just (Text.pack $ show level))
    , ("exp", attributeValue & avN .~ Just (Text.pack $ show exp))
    ]

-- doUserQuery :: DBInfo -> IO QueryResponse
-- doUserQuery DBInfo{..} = withDynamoDB env service region $ do
--     result <- send $ query tableName
--         & qExpressionAttributeValues .~ HashMap.singleton ":v1" (attributeValue & avS ?~ "USER#602853327784640532")
--         & qKeyConditionExpression ?~ "PK = :v1"

--     return $ do
--         valueAttr <- HashMap.lookup "level" (result ^. qrsItems)
--         valueNStr <- valueAttr ^. avN
--         parseInt valueNStr



doUserQuery :: DBInfo -> IO QueryResponse
doUserQuery DBInfo{..} = withDynamoDB env service region $ do
  result <- send $ query tableName
      & qExpressionAttributeValues .~ HashMap.singleton ":v1" (attributeValue & avS ?~ "USER#602853327784640532")
      & qKeyConditionExpression ?~ "PK = :v1"
  return result


doUpdateLevel :: Int -> Int -> Maybe (Int, Int)
doUpdateLevel level exp
    | level' > exp = Just (level, exp)
    | otherwise = Just ((level + (quot exp level')), (mod exp level'))
    where
        level' = level * 10

updateContext :: Int -> Text -> Maybe (HashMap.HashMap Text AttributeValue) ->  Maybe (HashMap.HashMap Text AttributeValue)
updateContext diceResult name path = 
    case path of
        Nothing -> do 
            return $ HashMap.fromList [ ("name", attributeValue & avS .~ Just name )
                            , ("level", toNumberAttr $ 1)
                            , ("exp", toNumberAttr $ diceResult)
                            ]
        Just context -> do 
            userName <- HashMap.lookup "name" (context)
            level' <- HashMap.lookup "level" (context)
            exp' <- HashMap.lookup "exp" (context)

            level <- level' ^. avN
            exp <- exp' ^. avN 

            levelInt <- parseInt level 
            expInt <- parseInt exp 
            
            (updateLevel, updateExp) <- doUpdateLevel levelInt (expInt + diceResult)

            return $ HashMap.fromList [ ("name",  userName )
                            , ("level", toNumberAttr $ updateLevel)
                            , ("exp", toNumberAttr $ updateExp)
                            ]
    where 
        toNumberAttr num = attributeValue & avN .~ Just (Text.pack . show $ num)

updateContext' :: Int -> Text -> Maybe (HashMap.HashMap Text AttributeValue) ->  Maybe (HashMap.HashMap Text AttributeValue)
updateContext' diceResult name path = 
    case path of
        Nothing -> do 
            return $ HashMap.fromList [ ("name", attributeValue & avS .~ Just name )
                            , ("level", toNumberAttr $ 1)
                            , ("exp", toNumberAttr $ diceResult)
                            ]
        Just context -> do 
            userName <- HashMap.lookup "name" (context)
            level' <- HashMap.lookup "level" (context)
            exp' <- HashMap.lookup "exp" (context)

            level <- level' ^. avN
            exp <- exp' ^. avN 

            levelInt <- parseInt level 
            expInt <- parseInt exp 
            
            (updateLevel, updateExp) <- doUpdateLevel levelInt (expInt + diceResult)

            return $ HashMap.fromList [ ("name",  userName )
                            , ("level", toNumberAttr $ updateLevel)
                            , ("exp", toNumberAttr $ updateExp)
                            ]
    where 
        toNumberAttr num = attributeValue & avN .~ Just (Text.pack . show $ num)

-- let id = Text.pack "602853327784640532"
-- putStrLn "doGetContext"
-- context <- getContext db id
-- atd <- doPutUserInfo db id "ATTENDANCE" (doUpdated context)
-- print atd

-- doUpdateAttendance :: DBInfo -> Maybe (HashMap.HashMap Text AttributeValue) -> IO ()
-- doUpdateAttendance DBInfo{..} = do
--     let id = Text.pack "id number"

--     putStrLn "doGetContext"
--     context <- getContext db id
--     return $ doPutUserInfo db id "ATTENDANCE" (doUpdated context)

doUpdated :: Maybe (HashMap.HashMap Text AttributeValue) -> Maybe (HashMap.HashMap Text AttributeValue)
doUpdated path = do
    case path of
        Just context -> do -- Maybe 
            userName <- HashMap.lookup "name" (context)
            attendance' <- HashMap.lookup "attendance" (context)

            -- attendance :: [Text]
            attendance <- Just $ attendance' ^. avSS
            -- changeTime attendance (Just $ length attendance)
            updated <- Just $ localToUTC attendance

            return $ HashMap.fromList [("name",  userName )
                            , ("attendance", attributeValue & avSS .~ updated)
                            ]
    where 
        toNumberAttr num = attributeValue & avN .~ Just (Text.pack . show $ num)

-- changeTime :: [Text] -> Int -> [Text]
-- changeTime (s:ss) 0 = 
-- changeTime (s:ss) len = do
--     | len > 0 = changeTime ((localTimeToUTC (hoursToTimeZone 9) s) : ss) (len-1)

-- changeSS :: [String] -> Int -> IO ()
-- changeSS (s:_) 0 = []
-- changeSS (s:ss) len
--     | (len > 0) = changeSS ("1":ss) (len-1)
--     | otherwise = print ss

-- today :: IO UTCTime
today :: IO LocalTime
today = do
    getCurrentTime' <- getCurrentTime
    -- return getCurrentTime' 
    -- getCurrentTime :: utctime 
    return $ utcToLocalTime (hoursToTimeZone 9) getCurrentTime'

today' :: IO UTCTime
today' = do
    getCurrentTime' <- getCurrentTime
    return getCurrentTime' 
    -- getCurrentTime :: utctime 
    -- return $ utcToLocalTime (hoursToTimeZone 9) getCurrentTime'

times = ["2022-07-30 14:02:37.294456777","2022-07-30 14:03:02.85413083","2022-07-30 15:01:33.044321211","2022-07-30 15:01:42.527053904","2022-07-31 18:31:32.808555769","2022-07-31 18:48:43.374597613","2022-07-31 18:52:49.875613045","2022-08-01 00:05:53.166820986"]
tss = map (\a -> (Text.pack a)) times
tss' = map (\a -> (Text.unpack a)) tss

-- -- LocalTime -> UTCTime
localToUTC :: [Text] -> [Text]
localToUTC atd = map (\a -> (Text.pack . show $ a)) $ upAtd
    where
        upAtd = map (\a -> (localTimeToUTC (hoursToTimeZone 9) (read . Text.unpack $ a))) atd



-- tt = test >>= prettyPrint .toJSON . head
-- test :: IO (Maybe [HashMap.HashMap Text AttributeValue])
-- test :: IO (Maybe (Text, [Text]))
-- test :: IO (Maybe [(Either String UTCTime, HashMap.HashMap Text AttributeValue)])
-- test :: IO (Maybe Int)
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



    let threadId = Text.pack "1031114387706085466"


    -- -- put items
    -- doPutMessages db threadId $ Program { pgName = "22st4", pgChapter = Ch1 }
    -- putStrLn "doPut compleat"
    -- 30677091998139174000094339137537 -> 1week first
    -- 30677092017305341092678563266560 -> 6week last

    -- -- create query items (test server)
    -- doPutMessages db threadId $ Program { pgName = "22st0", pgChapter = Ch1 }
    -- putStrLn "doPut' query compleat"

    -- -- delete items
    -- context <- getContextPK db threadId "THREAD" 
    -- doDeleteMessages db threadId "THREAD" $ filter findMassges $ context ^. qrsItems
    -- putStrLn "doDelete query compleat"


    -- doDeleteItem db id pk sk
    -- doDeleteItem db threadId "TEST" "00000000"
    -- putStrLn "delete Item"


    -- edit cron and start date
    context <- getContext db threadId "THREAD" "PROGRAM"
    let day = CalWee.fromWeekDate 2022 42 1
    let utcToday = UTCTime {utctDay = day, utctDayTime = 0}
    -- let it test -> take 5 $ utcGen (Text.pack "0 1 * * mon,wed,fri") utcToday
    -- let updated = editProgramInfo "22st4" (Text.pack "0 1 * * mon,wed,fri") (toSeconds utcToday) context
    let updated = editProgramInfo "22st4" "0 1 * * mon,wed,fri" (toSeconds utcToday) context
    _ <- doPutItem db threadId "THREAD" "PROGRAM" updated 
    putStrLn "edit compleat"




    -- context <- getContextPK db threadId "THREAD" -- thread
    -- context <- getContextPK db (Text.pack "1015503505806610452") "THREAD" -- chennnal

    -- 1662382376
    -- THREAD#1015503505806610452
    -- 1663124074331 / milli
    -- 1663327220

    -- let day = CalWee.fromWeekDate 2022 36 1
    -- let localToday = LocalTime {localDay = day, localTimeOfDay = TimeOfDay { todHour = 10, todMin = 2, todSec = 0}}
    -- let utcToday = localTimeToUTC (hoursToTimeZone 9) localToday
    -- let todayToMilli = (toSeconds utcToday) * 1000
    -- let today = getNumber (Number (scientific todayToMilli 1000))


    -- day' <- getCurrentTime
    -- (toSeconds day') * 1000
    -- let (Just headMsg) = headM todayMsg
    -- return headMsg
    -- return $ dayOfMessage' millisecond (context ^. qrsItems)
    -- return $ findTodayMessage' today 9 headMsg

    -- let (Just millisecond) = getNumber (Number 1665906043463)
    -- let (Just today) = quotMaybe (coefficient millisecond) 1000
    -- let (Just todayMsg) = dayOfMessage' millisecond (context ^. qrsItems)
    -- return $ createWeekMessage' todayMsg


    -- doPutUserInfo db (Text.pack "0000") "ATTENDANCE" userDummy
    -- putStrLn "test user dummy"

    -- userContext <- getContext db (Text.pack "0000") "USER" "ATTENDANCE"
    -- let updated = deleteLastDay userContext
    -- -- return updated
    -- doPutUserInfo db (Text.pack "0000") "ATTENDANCE" updated
    -- putStrLn "delete attendance at last day is compleat"

    -- 
    -- putStrLn "PutItem"
    -- doPutUserInfo db "001" $ doUpdateLevel "testUser" (15+20) 3
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

-- createImage :: (Int, Text) -> (HashMap.HashMap Text AttributeValue)
-- createImage (id, msg) = do
--     return $ HashMap.fromList [ ("id", attributeValue & avN .~ Just id )
--                             , ("image", attributeValue & avS .~ msg)
--                             ]

-- map ((length ch01)) ch01
--(createImage $ zip [0..] (img ch))



-- doPutMessages
doPutMessages :: DBInfo -> Text -> Program Program22st3Chapters -> IO ()
doPutMessages db threadId Program{ pgName = name, pgChapter = ch} -- = undefined 
    | ch == maxBound = do
        seconds <- getUnixMillis
        (Right num) <- generateUniqueId $ defaultConfig { allowableDowntime = seconds }

        doPutItem db threadId "THREAD" (show num) (createMessageItem (name ++ (show ch)) (msg ch) (img ch))
    | otherwise = do
        seconds <- getUnixMillis
        (Right num) <- generateUniqueId $ defaultConfig { allowableDowntime = seconds }

        doPutItem db threadId "THREAD" (show num) (createMessageItem (name ++ (show ch)) (msg ch) (img ch))
        doPutMessages db threadId (Program { pgName = name, pgChapter = (succ ch)})


doDeleteMessages :: DBInfo -> Text -> String -> [HashMap.HashMap Text AttributeValue] -> IO ()
doDeleteMessages db threadId pk [] = return ()
doDeleteMessages db threadId pk (ctx:ctxss) = do
    doDeleteItem db threadId pk sk
    doDeleteMessages db threadId pk ctxss
    where
        (Just sk) = getSK ctx
        -- add filter

findMassges :: HashMap.HashMap Text AttributeValue -> Bool
findMassges path 
    | sk == "PROGRAM" = False
    | otherwise = True
    where
        (Just sk) = getSK path


dummy :: HashMap.HashMap Text AttributeValue 
dummy = HashMap.fromList [
    ("name", attributeValue & avS .~ Just (Text.pack "testName"))
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







createWeekMessage' :: [(Either String UTCTime, HashMap.HashMap Text AttributeValue)] -> Maybe (Text, [Text])
createWeekMessage' path = do
    case headM path of
        Nothing -> Nothing
        Just (time, msg) -> do
            context <- HashMap.lookup "context" msg
            message' <- HashMap.lookup "message" (context ^. avM)
            images' <- HashMap.lookup "images" (context ^. avM)

            message <- message' ^. avS
            images <- Just $ images' ^. avSS
            
            return (message, images)

findTodayMessage' :: Integer -> Int -> (Either String UTCTime, HashMap.HashMap Text AttributeValue) -> Bool
findTodayMessage' milliS timeZone ((Right day), msg)
    -- | (date == msgDate) && (time > msgTime) && (time < msgTime') = True
    | (time > msgTime) && (time < msgTime') = True
    | otherwise = False
    where
        today = utcToLocalTime (hoursToTimeZone timeZone) (fromSeconds milliS)
        date = localDay today
        time = localTimeOfDay today
        
        msgDay = utcToLocalTime (hoursToTimeZone timeZone) day
        msgDate = localDay msgDay
        (msgTime, msgTime') = timeRange msgDay

timeRange :: LocalTime -> (TimeOfDay, TimeOfDay)
timeRange day = ((timeToTimeOfDay $ localTimeToDiffTime - 300), (timeToTimeOfDay $ localTimeToDiffTime + 300))
    where
        localTimeToDiffTime = timeOfDayToTime $ localTimeOfDay day

-- dayOfMessage' :: Scientific -> [HashMap.HashMap Text AttributeValue] -> Maybe [HashMap.HashMap Text AttributeValue]
-- dayOfMessage' :: Scientific -> [HashMap.HashMap Text AttributeValue] -> Maybe Int
dayOfMessage' :: Scientific -> [HashMap.HashMap Text AttributeValue] -> Maybe [(Either String UTCTime, HashMap.HashMap Text AttributeValue)]
-- dayOfMessage' milliS path = (zip dayOfMWF weekMessage)
dayOfMessage' milliS path = do 

    info <- headM programInfo 
    startDay <- getStartDay info
    cron <- getProgramCron info
    timeZone <- getProgramTimezone info

    today <- quotMaybe (coefficient milliS) 1000

    -- return $ timeZone
    -- return $ zip (utcGen cron $ fromSeconds startDay) weekMessage 
    return $ filter (\x -> (findTodayMessage' today timeZone x)) $ zip (utcGen cron $ fromSeconds startDay) weekMessage 

    where
        weekMessage = filter findMessages path
        programInfo = filter findProgram path

        -- Just startDay = do 
        --     info <- headM programInfo 
        --     getStartDay info

        -- (Just cron) = do 
        --     info <- headM programInfo 
        --     getProgramCron info

        -- -- dayOfMWF = filterMWF' startDay

        -- (Just today) = quotMaybe (coefficient milliS) 1000

        getStartDay :: HashMap.HashMap Text AttributeValue -> Maybe Integer
        getStartDay path = do
            context <- HashMap.lookup "context" path
            program_start_date' <- HashMap.lookup "program_start_date" (context ^. avM)
            sd <- Text.unpack <$> program_start_date' ^. avN
            readMaybe sd

        getProgramCron :: HashMap.HashMap Text AttributeValue -> Maybe Text
        getProgramCron path = do
            context <- HashMap.lookup "context" path
            program_cron' <- HashMap.lookup "program_cron" (context ^. avM)
            cron <- program_cron' ^. avS
            return cron

        getProgramTimezone :: HashMap.HashMap Text AttributeValue -> Maybe Int
        getProgramTimezone path = do
            context <- HashMap.lookup "context" path
            program_timezone' <- HashMap.lookup "program_timezone" (context ^. avM)
            tz <- Text.unpack <$> program_timezone' ^. avN
            readMaybe tz


utcGen :: Text -> UTCTime -> [Either String UTCTime]
utcGen cron today = case gen of 
                        Right days -> days 
                        Left e -> [Left e]

    where
        gen = do
            expr <- parseCronSchedule cron
            nextDay <- match expr today
            return $ Right nextDay : utcGen cron nextDay 


        match cronExpr td = case nextMatch cronExpr today of 
                                Nothing -> Left $ (show $ today) <> " is not match in cron expression"
                                Just day -> Right day


updateAttendance :: Text -> Text -> Integer -> Maybe (HashMap.HashMap Text AttributeValue) ->  Maybe (HashMap.HashMap Text AttributeValue)
updateAttendance userName userId today path = 
    case path of
        Nothing -> do 
            return $ HashMap.fromList [ ("name", attributeValue & avS .~ Just userName)
                            , ("id", attributeValue & avS .~ Just userId)
                            , ("attendance", attributeValue & avNS .~ [(Text.pack . show $ today)])
                            ]
        Just context -> do -- Maybe 
            name' <- HashMap.lookup "name" (context)
            id' <- HashMap.lookup "id" (context)
            attendance' <- HashMap.lookup "attendance" (context)

            name <- Just $ name' ^. avS
            id <- Just $ id' ^. avS
            attendance <- Just $ attendance' ^. avNS

            return $ HashMap.fromList [ ("name", attributeValue & avS .~ name)
                            , ("id", attributeValue & avS .~ id)
                            , ("attendance", attributeValue & avNS .~ ((Text.pack . show $ today): attendance))
                            ]


deleteLastDay :: Maybe (HashMap.HashMap Text AttributeValue) ->  Maybe (HashMap.HashMap Text AttributeValue)
-- deleteLastDay :: Maybe (HashMap.HashMap Text AttributeValue) -> Maybe [Text]
deleteLastDay path = 
    case path of
        Just context -> do -- Maybe 
            name' <- HashMap.lookup "name" (context)
            id' <- HashMap.lookup "id" (context)
            attendance' <- HashMap.lookup "attendance" (context)

            name <- Just $ name' ^. avS
            id <- Just $ id' ^. avS

            let attendance = findLastDay . sort . map maybeInt $ readMaybe . Text.unpack <$> attendance' ^. avNS
            -- let attendance = sort . map maybeInt $ readMaybe . Text.unpack <$> attendance' ^. avNS
            -- return attendance

            return $ HashMap.fromList [ ("name", attributeValue & avS .~ name)
                            , ("id", attributeValue & avS .~ id)
                            , ("attendance", attributeValue & avNS .~ attendance)
                            ]

-- findLastDay :: [Text] -> [Text]
findLastDay :: [Integer] -> [Text]
findLastDay attT = map (Text.pack . show) attI
    where
        lastDay = lastM attT
        attI = filter (filterLastDays (maybeInt lastDay)) attT
        

filterLastDays :: Integer -> Integer -> Bool
filterLastDays lastDay day
    | lastLocalDay == localD = False
    | otherwise = True
    where
        lastLocalDay = localDay $ utcToLocalTime (hoursToTimeZone 9) (fromSeconds lastDay)
        localD = localDay $ utcToLocalTime (hoursToTimeZone 9) (fromSeconds day)



-- 1663858206
userDummy :: Maybe (HashMap.HashMap Text AttributeValue)  
userDummy = return $ HashMap.fromList [ 
    ("name", attributeValue & avS .~ Just "Baaaam")
    , ("id", attributeValue & avS .~ Just "602853327784640532")
    , ("attendance", attributeValue & avNS .~ dates)
    ]
    where
        dates = map (\x -> (Text.pack . show $ x)) [1663858206,1662826056,1663336375,1664547414,1663425386,1665157044,1662738507,1662569336,1664285472,1664464884,1663169000,1663937097,1664723125,1665490345,1663773656,1665414634,1664378771,1662903162,1662996462,1664981811,1665242167,1665332640,1664799393,1664032814,1663258559,1665064939,1663603712,1662658106,1663082369,1662339600,1663511375,1664635232,1662418800,1664117111,1662475284,1664196830,1664892521,1663687921]



-- [1665223352,1665224415,1662047677,1662300699,1665224403]
-- [1665223352,1662300699,1665224403,1665224415,1662047677,1665595884,1665595962]
-- [1665223352,1665224415,1662047677,1662300699,1665224403]

-- ["1665223352","1665224415","1662047677","1662300699","1665224403"]
-- ["1662300699","1662047677"]
-- ["1662300699","1665597055","1662047677"]

-- 1663331177
-- filterMWF' :: Maybe Integer -> [LocalTime]
-- filterMWF' (Just seconds) = oneDay ++ twoDay
--     where
--         localDate = utcToLocalTime (hoursToTimeZone 9) (fromSeconds seconds)
--         day = localDay localDate
--         timeH = todHour $ localTimeOfDay localDate
--         oneDay = fmap (\x ->  LocalTime { localDay = day, localTimeOfDay = TimeOfDay { todHour = x, todMin = 30, todSec = 0}}) $ take 9 [timeH..]
--         twoDay = fmap (\x ->  LocalTime { localDay = (succ day), localTimeOfDay = TimeOfDay { todHour = x, todMin = 30, todSec = 0}}) $ take 9 [timeH..]




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

