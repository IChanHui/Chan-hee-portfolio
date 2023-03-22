{-# LANGUAGE FlexibleContexts #-}


-- All of amazonka APIs use Data.Text.Text by default which is nice
{-# LANGUAGE OverloadedStrings #-}

-- Allows record fields to be expanded automatically
{-# LANGUAGE RecordWildCards #-}

module DbAutoSortTest where

import Data.Text (Text, pack)
import           Control.Monad.Trans.AWS ( Region(..))
import           Control.Lens ((<&>), (^.), (.~), (&), (?~), set)
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
import qualified Data.HashMap.Strict as HashMap (fromList, lookup, singleton, HashMap)

import DynamoDb 

-- updateAttendance :: Text -> Text -> Integer -> Maybe (HashMap.HashMap Text AttributeValue) ->  Maybe (HashMap.HashMap Text AttributeValue)
-- updateAttendance userName userId today path = 
--     case path of
--         Nothing -> do 
--             return $ HashMap.fromList [ ("name", attributeValue & avS .~ Just userName)
--                             , ("id", attributeValue & avS .~ Just userId)
--                             , ("attendance", attributeValue & avNS .~ [(Text.pack . show $ today)])
--                             ]
--         Just context -> do -- Maybe 
            
--             name <- Just $ name' ^. avS
--             id <- Just $ id' ^. avS
--             attendance <- Just $ attendance' ^. avNS

--             return $ HashMap.fromList [ ("name", attributeValue & avS .~ name)
--                             , ("id", attributeValue & avS .~ id)
--                             , ("attendance", attributeValue & avNS .~ ((Text.pack . show $ today): attendance))


test :: IO ()
test = do 
    db <- getDBInfo LoggingDisabled (AWS Seoul)
    let id = "602853327784640532"
    let name = "Baaaam"
    let key = "data" :: Text

    -- let numbers = map (\(k,v) -> attributeValue & avM .~ (HashMap.fromList [(pack (show k), attributeValue & avN .~ (Just $ pack (show v))  )])) $ zip [0..] [9,8,7,6,5,4,3,2,1]
    let numbers = map (pack . show) [9,8,7,6,5,4,3,2,1]

    let context = HashMap.fromList [ 
                -- (key, attributeValue & avNS .~ numbers)
                (key, attributeValue & avSS .~ numbers)
            ]
    
    doPutItem db id "USER" "TEST" (Just context)
    return ()



-- createMembers :: [Text] -> Maybe (HashMap.HashMap Text AttributeValue)
-- createMembers numbers  = do
--     return $ HashMap.fromList [ ("name", attributeValue & avNS .~ Just argUserName )
--                             ]


-- maybeContext <- getContext db id "USER" "ATTENDANCE"
-- _ <- doPutUserInfo db id "ATTENDANCE" updated

-- {"name":{"S":"Baaaam"},"attendance":{"NS":["1662300699","1665597055","1662047677"]},"id":{"S":"602853327784640532"}}

-- userDummy :: Maybe (HashMap.HashMap Text AttributeValue)  
-- userDummy = return $ HashMap.fromList [ 
--     ("attendance", attributeValue & avNS .~ dates)
--     ]


-- {"program_locale":{"S":"en-US"},
-- "operator_name":{"S":"Baaaam"},
-- "program_timezone":{"N":"9"},
-- "program_cron":{"S":"0/20 2-12 * * *"},
-- "program_thread_id":{"S":"1019486176819810374"},
-- "operator_id":{"S":"602853327784640532"},
-- "members":{"L":[{"M":{"name":{"S":"Baaaam"},
-- "id":{"S":"602853327784640532"}}}]},
-- "program_name":{"S":"22st3"},
-- "program_start_date":{"N":"1665723600"},
-- "limits":{"N":"0"}
-- }

-- [ ("operator_name", attributeValue & avS .~ Just userName )
-- , ("operator_id", attributeValue & avS .~ Just userId)
-- -- , (argUserName, attributeValue & avS .~ Just argUserId)
-- , ("members", attributeValue & avL .~ [(attributeValue & avM .~ member)])
-- , ("program_name", attributeValue & avS .~ Just (Text.pack "22st3"))
-- , ("program_thread_id", attributeValue & avS .~ Just threadId)
-- , ("program_locale", attributeValue & avS .~ Just locale)
-- , ("program_timezone", attributeValue & avN .~ Just (Text.pack . show . toTimezone $ locale))
-- , ("limits", attributeValue & avN .~ Just (Text.pack . show $ 0))
-- ]





