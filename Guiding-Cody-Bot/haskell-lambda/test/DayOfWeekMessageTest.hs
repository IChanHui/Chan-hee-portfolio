{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module DayOfWeekMessageTest where 

import           Control.Lens ((<&>), (^.), (.~), (&), (?~), set)
import           Data.Aeson
import           Data.DateTime (fromGregorian, toSeconds, fromSeconds)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup, singleton, HashMap)
import           Data.Text (Text)
import qualified Data.Text as Text (null, pack, unpack)
import           Data.Time hiding (fromGregorian)
import           Data.Time.Calendar hiding (fromGregorian)
import           Data.Time.Calendar.WeekDate as CalWee 
import           Data.Scientific
import           Database.Blacktip
import           Database.Blacktip.Types
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
import           Text.Read (readMaybe)

import MaybeFunctionTest


-- data Journaling = Ch1 
--                 | Ch2 
--                 | Ch3 
--                 | Ch4 
--                 | Ch5 | Ch6 | Ch7 | Ch8 | Ch9 | Ch10 | Ch11 | Ch12 | Ch13 | Ch14 | Ch15 | Ch16 | Ch17 | Ch18 
--     deriving (Enum, Show, Bounded, Eq)

type ProgramName = String 

class (Enum c, Bounded c, Eq c, Show c) => Chapter c where 

instance Chapter Program22st3Chapters where 


data Program a = Program { pgName :: ProgramName 
                       , pgChapter :: a
                       } deriving Show


    -- deriving (Enum, Show, Bounded, Eq)
-- maxBound :: Program22st3ChaptersMsg

data Program22st3Chapters = Ch1 | Ch2 | Ch3 | Ch4 | Ch5 | Ch6 | Ch7 | Ch8 | Ch9 | Ch10 | Ch11 | Ch12 | Ch13 | Ch14 | Ch15 | Ch16 | Ch17 | Ch18 
    deriving (Enum, Show, Bounded, Eq)


findProgram :: HashMap.HashMap Text AttributeValue -> Bool
findProgram path 
    | getSK == (Just "PROGRAM") = True
    | otherwise = False
    where
        getSK :: Maybe Text
        getSK = do
            sk' <- HashMap.lookup "SK" path
            sk <- sk' ^. avS
            return sk

findMessages :: HashMap.HashMap Text AttributeValue -> Bool
findMessages path 
    | getSK == (Just "PROGRAM") = False
    | otherwise = True
    where
        getSK :: Maybe Text
        getSK = do
            sk' <- HashMap.lookup "SK" path
            sk <- sk' ^. avS
            return sk



filterMWF :: Maybe Integer -> [LocalTime]
filterMWF (Just seconds) = map (\ld -> LocalTime { localDay = ld, localTimeOfDay = TimeOfDay { todHour = 10, todMin = 0, todSec = 0}}) getMWF 
    where
        localDate = utcToLocalTime (hoursToTimeZone 9) (fromSeconds seconds)
        day@(y, m, d) = toWeekDate $ localDay localDate
        gregCal = [(CalWee.fromWeekDate y m d) .. (CalWee.fromWeekDate y (m+6) d)]
        getMWF = filter getDayOfWeek gregCal

        getDayOfWeek :: Day -> Bool
        getDayOfWeek day
            | (dayOfWeek day == Monday) = True
            | (dayOfWeek day == Wednesday) = True 
            | (dayOfWeek day == Friday) = True
            | otherwise = False

findTodayMessage :: Integer -> (LocalTime, HashMap.HashMap Text AttributeValue) -> Bool
findTodayMessage milliS (day, msg) 
    | (date == msgDate) && (msgTime < time) && (time < msgTime') = True
    | otherwise = False
    where
        today = utcToLocalTime (hoursToTimeZone 9) (fromSeconds milliS)
        date = localDay today
        time = localTimeOfDay today
        msgDate = localDay day
        msgTime = TimeOfDay { todHour = 9, todMin = 55, todSec = 0}
        msgTime' = TimeOfDay { todHour = 10, todMin = 5, todSec = 0}


-- milliseconds = coefficient 1.662792217602e12
-- startDay = 1661960028
-- [HashMap.HashMap Text AttributeValue]
-- testFind :: Maybe Scientific -> [HashMap.HashMap Text AttributeValue] -> [LocalTime] 
dayOfMessage :: Scientific -> [HashMap.HashMap Text AttributeValue] -> [(LocalTime, HashMap.HashMap Text AttributeValue)]
-- dayOfMessage :: Maybe Scientific -> [HashMap.HashMap Text AttributeValue] -> Maybe Integer
dayOfMessage milliS path = filter (\x -> (findTodayMessage today x)) (zip dayOfMWF weekMessage)
-- dayOfMessage (Just milliS) path = (zip dayOfMWF weekMessage)
-- dayOfMessage (Just milliS) path = startDay
    where
        weekMessage = filter findMessages path
        programInfo = filter findProgram path
        startDay = do 
            info <- headM programInfo 
            getStartDay info

        dayOfMWF = filterMWF startDay

        (Just today) = quotMaybe (coefficient milliS) 1000

        getStartDay :: HashMap.HashMap Text AttributeValue -> Maybe Integer
        getStartDay path = do
            context <- HashMap.lookup "context" path
            program_start_date' <- HashMap.lookup "program_start_date" (context ^. avM)
            sd <- Text.unpack <$> program_start_date' ^. avN
            readMaybe sd



createWeekMessage :: [(LocalTime, HashMap.HashMap Text AttributeValue)] -> Maybe (Text, [Text])
createWeekMessage path = do
    case headM path of
        Nothing -> Nothing
        Just (time, msg) -> do
            context <- HashMap.lookup "context" msg
            message' <- HashMap.lookup "message" (context ^. avM)
            images' <- HashMap.lookup "images" (context ^. avM)

            message <- message' ^. avS
            images <- Just $ images' ^. avSS
            
            return (message, images)

getNumber :: Value -> Maybe Scientific
getNumber (Number x) = Just x
getNumber _ = Nothing

miniSeconds :: Value
miniSeconds = Number 1662339649570
-- miniSeconds = Number 1662087617398

getTimestamp :: HashMap.HashMap Text AttributeValue -> Maybe [Maybe Integer]
getTimestamp userContext = do 
    timestamp' <- HashMap.lookup "timestamp" userContext
    return $ readMaybe . Text.unpack <$> timestamp' ^. avNS 

-- imageDummy :: HashMap.HashMap Text AttributeValue 
-- imageDummy = HashMap.fromList [ 
--     ("1662253200", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg"]))
--     , ("1662512400", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]))
--     , ("1662771600", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-3.jpg"]))

--     , ("1662944400", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg"]))
--     , ("1663117200", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]))
--     , ("1663290000", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg"]))

--     , ("1663549200", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg"]))
--     , ("1663722000", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]))
--     , ("1663894800", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg"]))
    
--     , ("1664154000", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg"]))
--     , ("1664326800", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]))
--     , ("1664499600", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg"]))
    
--     , ("1664758800", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg"]))
--     , ("1664931600", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]))
--     , ("1665104400", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg"]))
    
--     , ("1665363600", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg"]))
--     , ("1665536400", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]))
--     , ("1665709200", attributeValue & avSS .~ (toText ["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg", "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg"]))
--     ]
--     where
--         toText x = map Text.pack x

messageDummy :: HashMap.HashMap Text AttributeValue 
messageDummy = HashMap.fromList [ 
    ("1662253200", attributeValue & avSS .~ (toText ["@everyone \n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n한 주 동안도 함께 응원하고 격려하며 평온하게 지내보도록 해요 :open_hands:\n\n먼저 1주차 이야기와 1주차에 사용할 메모지 목록을 보내드려요.\n매주가 시작되는 월요일에는 그 주에 진행할 이야기와 메모지 목록을 보내드린답니다 :smiling_face_with_3_hearts:\n\n그리고 월요일 주제도 함께 나눠볼게요! 잘 읽고 오늘밤 주무시기 전에 끼적인거 인증해주심 되어요오오오 :rabbit:\n그럼 오늘 하루도 행복하세요:orange_heart:\n\n오늘의 가이딩코디의 이야기 공유드립니다 :rabbit: \n\n:point_down: 오늘의 끼적이기 주제 보기 \n https://resonance2heart.notion.site/1-e88afc5dbde7472ea9865197dcd861c4"]))
    , ("1662512400", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 지칠 수 있는 수요일이지만 무리하지말고 조금은 쉬어가는 그런 날이 되셨으면 좋겠어요! \n자 오늘도 수요일 주제를 들고 왔는데요, \n바로 '나 자신을 관찰하고 끼적이기' 로 활동을 해볼거에요.\n소중한 다른 사람을 돌봐주듯이 나 자신을 돌봐주며 지내보는거죠.\n\n오늘의 가이딩코디의 이야기를 들어볼게요:rabbit:\n\n:point_down: 오늘의 끼적이기 주제 보기\n https://resonance2heart.notion.site/1-e02eb481dbbf4f8da11a159291afcb95"]))
    , ("1662771600", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요 :smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요, \n바로 '감사하기' 랍니다. 이건 너무나 익숙한 주제이기도 하죠?\n\n감사하기가 좋다는건 누구나 다 아는 사실일텐데요. 정확히 어떻게 좋은지 어떻게 감사하면 좋을지에 대해서 가이딩 코디의 이야기를 들어볼게요.\n\n소중한 다른 사람을 돌봐주듯이 나 자신을 돌봐주며 지내보는거죠.\n\n오늘도 가이딩코디의 이야기를 들어볼게요:rabbit:\n\n:point_down: 오늘의 끼적이기 주제 보기\n https://resonance2heart.notion.site/1-1b3409abaef946b285c976389c1c89ac"]))

    , ("1662944400", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n이번주는 어떤 한주를 보내고 싶으신가요? 함께 원하는대로 만들어가봐요.\n\n먼저 2주차 이야기와 사용할 메모지 목록을 보내드려요.\n2주차 주제는 ”리틀코디에게 사랑주기” 주제로 진행합니다.\n\n오늘 월요일 주제는요,\n사랑해 메모지에 '내가 좋아하는 것' 끼적이기에요. 내가 좋아하는 것을 샅샅이 뒤져 메모지에 끼적이는거죠.\n내가 좋아하는 것을 알고 나에게 선물을 해주는 연습을 해볼거에요.\n\n:point_down: 오늘의 가이딩코디의 이야기 공유드립니다 :rabbit:\n https://resonance2heart.notion.site/2-a2c46a18d65b40e9bf2dc228ea265af8 \n\n그럼 오늘 하루도 행복하세요:orange_heart:"]))
    , ("1663117200", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 수요일 주제를 들고 왔는데요:heart: \n바로 '좋은 기억들 끼적여보기' 활동을 해볼거에요.\n다들 살아오면서 자신이 가진 좋은 기억들이 있을거에요. 많든 적든요. \n그 기억을 꺼내서 지금을 살아가는 원동력으로 만들어 볼게요.\n\n:point_down: 오늘의 가이딩코디의 이야기를 들어볼게요\n https://resonance2heart.notion.site/2-94627d16cdcc406685e22f3b8f28c2ff"]))
    , ("1663290000", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴입주민 여러분!\n\n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요:smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요,  바로 '자신의 장점을 끼적이기' 랍니다. \n\n:point_down: 오늘의 가이딩코디의 이야기를 들어볼게요\n https://resonance2heart.notion.site/2-d75f6c7a133d44a29730c75945a5069b"]))

    , ("1663549200", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n이번주는 어떤 한주를 보내고 싶으신가요? \n이번주는 확언을 통해 내가 만들어가는 삶을 더 가까이 당겨볼까요? \n\n먼저 3주차 이야기와 사용할 메모지 목록을 보내드려요. 3주차 주제는 \n”리틀코디와 깊은 대화하기” 라는 주제로 진행합니다.\n\n오늘 월요일 끼적이기는\n\n굴 메모지에 '나의 가득한 생각' 끼적이기에요. \n머리속에서 끊임없이 나를 괴롭히는 부정적인 생각이나 도움이 안되는 생각에 대해 써보는거죠.\n참고로 굴 메모지는 코디의 굴 입구에 위치한 굴 벽을 상징하기도 한답니다 :smiling_face_with_3_hearts: \n굴 입구 벽은 머리 속에 생각나는걸 휘갈겨 쓰는 공간이기도 해요.\n\n:point_down: 오늘의 가이딩코디의 이야기 공유드립니다:rabbit:\n https://resonance2heart.notion.site/3-4478e80edd134d91b85dcec004ef8f61\n\n그럼 오늘 하루도 행복하세요:orange_heart:"]))
    , ("1663722000", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 수요일 주제를 들고 왔는데요!! \n바로 '느낌 적어보기' 활동을 해볼거에요.\n아니 왜 갑자기 무슨 느낌을 적냐고요? 이 느낌은 바로 \n\n**'우리가 월요일에 적었던 내 머리속을 가득채우고 있는 생각과 감정을 굴 벽에 끼적이기한 것에 대한 느낌'**을 적는걸 의미해요.\n\n자자 :point_down: 오늘의 가이딩코디의 이야기 공유드립니다:rabbit:\n https://resonance2heart.notion.site/3-e49d6e904f4942b0bf2be27e65ad4d88"]))
    , ("1663894800", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 금요일이에요. 평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 밤이 되길 바라요:smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요, \n바로 '나은 방향에 대한 생각 쓰기' 랍니다. \n\n이것 또한 앞에 **월요일, 수요일에 적으신 머리 속에 가득한 나의 생각과 감정에 대해 더 나은 방향으로 가려면 어떻게 해야할지에 대해 적어보는거에요.**\n\n:point_down: 오늘도 가이딩 코디의 이야기 들어볼게요.\n https://resonance2heart.notion.site/3-383eaa6bfce54d949771e3580925eda4"]))
    
    , ("1664154000", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n\n벌써 저희 4주차나 됐어요! 자신을 돌아보는데에 도움이 되시길 바라요!\n자 이번 한주는 어떻게 보내볼까요? 우리가 만들어보죠!\n\n이번 4주차 이야기와 사용할 메모지 목록을 보내드려요. \n4주차 주제는 ”리틀코디에게 용기주기(내면 토닥이기)” 라는 주제로 진행합니다.\n\n오늘 월요일 주제는요,\n할 수 있어! 포스트잇에 '두렵고 자신없는 것' 끼적이기에요. \n살면서 두려운게 없을 수는 없잖아요. 두렵지는 않아도 좀 걱정되는 것도 있고요.\n그런 것들을 써보는 시간이 되겠습니다:relaxed: \n\n:point_down: 자, 가이딩 코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/4-ac53fccf88284867b205270ebf932788"]))
    , ("1664326800", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 한 주의 중간같은 느낌인만큼 조금은 지치실텐데\n언제나 저희 토끼굴22번길은 여러분을 응원하고 격려드립니다아:rabbit:\n이번주 주제는 바로 '두려운 것에 대한 대응방안과 격려의 말 쓰기' 활동을 해볼거에요.\n지난 월,화요일에 두려운 것을 썼기에 이제 이 두려움을 내 일부로 수용하며 격려한 후,\n더 나은 삶을 살기 위한 대응방안을 써보는 시간을 갖는거죠.\n\n:point_down: 자자, 오늘의 가이딩코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/4-62d134fc0db0494bb9da929ab304ecf6"]))
    , ("1664499600", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 금요일이에요. 평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 밤이 되길 바라요:smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요, \n바로 '힘이 될 메세지 찾아서 적어보기' 입니다. \n이것 또한 지난 수요일에 적으신 격려의 말과 대응방안과 비슷하지만 이번에는 인터넷이나 다른 사람들의 말을 빌려 나에게 힘이 될 만한 메세지를 수집하고 내 마음 속에 내 것으로 가져오는 시간이 될거에요.\n\n:point_down: 오늘도 가이딩 코디의 이야기 들어볼게요.\n https://resonance2heart.notion.site/4-23010ca0ad7a4fb89e27e62d6c8075ba"]))
    
    , ("1664758800", attributeValue & avSS .~ (toText ["@everyone\n\n오늘은 월요일 한 주의 시작이에요:relaxed::two_hearts:\n한 주 동안도 함께 응원하면서 평온하게 지내보도록 해요!\n먼저 5주차 주제는 “인간관계' 입니다. 자신의 인간관계를 바라보고 진행할거에요.\n\n오늘 월요일 주제를 보고 함께 끼적여보아요.\n자기전에 올려주시면 됩니다:heart:\n\n https://resonance2heart.notion.site/5-29b95378a9f442f5bc406a4bd32a33f5"]))
    , ("1664931600", attributeValue & avSS .~ (toText ["@everyone\n\n오늘 하루도 평안하셨나요?:smiling_face_with_3_hearts: 드디어! 수요일 주제를 들고 왔는데요,\n바로 '나를 힘들게 한 사람에게 편지쓰기' 로 끼적여볼거에요.\n월요일 주제였던 나를 힘들게 한 사람들이나 특징에 대해 써봤는데 그 사람들에게\n하지 못했던 말을 해보는 시간인거죠.\n\n:point_down: 가이딩코디의 이야기 한번 들어보도록 할게요\n https://resonance2heart.notion.site/5-4801696e7c604e6696d28771e20c9548"]))
    , ("1665104400", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요. 오늘은 금요일이에요. \n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요:smiling_face_with_3_hearts:\n오늘도 주제를 들고 왔는데요, \n바로 '불편한 사람에게 대응하는 방법 끼적이기' 랍니다. \n\n여태까지 불편한 사람을 떠올려보고 특징을 적으며 그 사람에게 편지쓰기 까지 했었는데요. \n이렇게 욕하고 나면 끝이었음 좋겠지만, 우리가 살아가야할 삶이 긴 만큼 비슷한 사람을 만나면 현명하게 대처하는 방법을 익혀두고 연습해야할 필요가 있어요. 그래야 같은 상처를 또다시 받지 않고 넘어갈 수 있을테니까요.\n\n:point_down: 자. 가이딩코디의 이야기를 들어볼게요.\nhttps://resonance2heart.notion.site/5-2e276321a5d14245b37b55402be08265"]))
    
    , ("1665363600", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n이번주는 아쉽게도 끼적이기 프로그램 마지막 주차에요.\n마지막인만큼 좀 더 열심히 참여해주시고 자신을 사랑하고 위한 노력\n앞으로도 계속 하기에요!\n\n먼저 6주차 이야기와 사용할 메모지 목록을 보내드려요. 6주차 주제는 \n”리틀코디가 그리는 미래” 라는 주제로 진행합니다.\n\n오늘 월요일 주제는요,\n따뜻한 하루 메모지 에 '버킷리스트 끼적이기' 에요. 앞으로 다가올 나는 무엇을 하고 싶은지\n자유롭게 적고 이루기 위해서는 어떻게 해야할지 적어보는거죠!\n\n:point_down: 자, 가이딩 코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/8-9f31325d955f4dd4ab6440beab7a8c2c"]))
    , ("1665536400", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 지치시겠지만 좀 더 마음을 가다듬고\n편안한 하루의 시간이 되시길 진심으로 바라요!\n\n이번주는 '거과의 기억 바꾸기' 활동을 해볼거에요.\n갑자기 과거의 기억을 바꾼다니. 이 뭔 SF 영화같은 말인가 하실텐데요!\n이게 실제로 가능합니다.\n\n오늘의 가이딩코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/8-387c40c187f54dc09c9ce7371847b4e8"]))
    , ("1665709200", attributeValue & avSS .~ (toText ["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요:smiling_face_with_3_hearts:\n그리고 오늘 마지막 주제에요! >ㅁ<  뭔가 아쉽지만..그만큼 마지막을 멋지게 진행해보시죠!\n바로 '미래를 심상화 하기' 랍니다. 어엇 심상화라니, 좀 낯설고 어려워 보이시죠?  \n\n:point_down: 가이딩 코디의 이야기 들어볼게요.\n https://resonance2heart.notion.site/8-16427d2357634c478fc40975866f4971"]))
    ]
    where
        toText x = map Text.pack x



createMessageItem :: String -> Text -> [Text] -> Maybe (HashMap.HashMap Text AttributeValue)
createMessageItem name msg img = return $ HashMap.fromList [ 
    ("name", attributeValue & avS .~ Just (Text.pack name))
    , ("message", attributeValue & avS .~ Just msg)
    , ("images", attributeValue & avSS .~ img)
    ]
--programMessage


startDay = utcToLocalTime (hoursToTimeZone 9) (fromGregorian 2022 9 5 1 0 0)
day@(y, m, d) = toWeekDate . localDay $ startDay
gregCalMon = [(CalWee.fromWeekDate y 36 1), (CalWee.fromWeekDate y 37 1) .. (CalWee.fromWeekDate y (36+5) 1)]
gregCalWed = [(CalWee.fromWeekDate y 36 3), (CalWee.fromWeekDate y 37 3) .. (CalWee.fromWeekDate y (36+5) 3)]
gregCalFri = [(CalWee.fromWeekDate y 36 5), (CalWee.fromWeekDate y 37 5) .. (CalWee.fromWeekDate y (36+5) 5)]

localListMon = map (\ld -> LocalTime { localDay = ld, localTimeOfDay = TimeOfDay { todHour = 10, todMin = 0, todSec = 0}}) gregCalMon
localListWed = map (\ld -> LocalTime { localDay = ld, localTimeOfDay = TimeOfDay { todHour = 10, todMin = 0, todSec = 0}}) gregCalWed
localListFri = map (\ld -> LocalTime { localDay = ld, localTimeOfDay = TimeOfDay { todHour = 10, todMin = 0, todSec = 0}}) gregCalFri
-- localDay, localTimeOfDay 
-- toWeekDate CalWee.fromWeekDate
-- toSeconds fromSeconds

-- map (dayOfWeek . localDay . utcToLocalTime (hoursToTimeZone 9) . fromSeconds) attendanceToSeconds

lm = ["2022-09-05 10:00:00","2022-09-12 10:00:00","2022-09-19 10:00:00","2022-09-26 10:00:00","2022-10-03 10:00:00","2022-10-10 10:00:00"]
lw = ["2022-09-07 10:00:00","2022-09-14 10:00:00","2022-09-21 10:00:00","2022-09-28 10:00:00","2022-10-05 10:00:00","2022-10-12 10:00:00"]
lf = ["2022-09-09 10:00:00","2022-09-16 10:00:00","2022-09-23 10:00:00","2022-09-30 10:00:00","2022-10-07 10:00:00","2022-10-14 10:00:00"]

lms = [1662253200, 1662944400, 1663549200, 1664154000, 1664758800, 1665363600]
lws = [1662512400, 1663117200, 1663722000, 1664326800, 1664931600, 1665536400]
lfs = [1662771600, 1663290000, 1663894800, 1664499600, 1665104400, 1665709200]

ts :: [Text]
ts = map (\x -> (Text.pack . show $ x)) [1662253200, 1662512400, 1662771600, 1662944400, 1663117200, 1663290000, 1663549200, 1663722000, 1663894800, 1664154000, 1664326800, 1664499600, 1664758800, 1664931600, 1665104400, 1665363600, 1665536400, 1665709200]




msg :: Program22st3Chapters -> Text 
msg Ch1 = Text.pack "@everyone \n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n한 주 동안도 함께 응원하고 격려하며 평온하게 지내보도록 해요 :open_hands:\n\n먼저 1주차 이야기와 1주차에 사용할 메모지 목록을 보내드려요.\n매주가 시작되는 월요일에는 그 주에 진행할 이야기와 메모지 목록을 보내드린답니다 :smiling_face_with_3_hearts:\n\n그리고 월요일 주제도 함께 나눠볼게요! 잘 읽고 오늘밤 주무시기 전에 끼적인거 인증해주심 되어요오오오 :rabbit:\n그럼 오늘 하루도 행복하세요:orange_heart:\n\n오늘의 가이딩코디의 이야기 공유드립니다 :rabbit: \n\n:point_down: 오늘의 끼적이기 주제 보기 \n https://resonance2heart.notion.site/1-e88afc5dbde7472ea9865197dcd861c4"
msg Ch2 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 지칠 수 있는 수요일이지만 무리하지말고 조금은 쉬어가는 그런 날이 되셨으면 좋겠어요! \n자 오늘도 수요일 주제를 들고 왔는데요, \n바로 '나 자신을 관찰하고 끼적이기' 로 활동을 해볼거에요.\n소중한 다른 사람을 돌봐주듯이 나 자신을 돌봐주며 지내보는거죠.\n\n오늘의 가이딩코디의 이야기를 들어볼게요:rabbit:\n\n:point_down: 오늘의 끼적이기 주제 보기\n https://resonance2heart.notion.site/1-e02eb481dbbf4f8da11a159291afcb95"
msg Ch3 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요 :smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요, \n바로 '감사하기' 랍니다. 이건 너무나 익숙한 주제이기도 하죠?\n\n감사하기가 좋다는건 누구나 다 아는 사실일텐데요. 정확히 어떻게 좋은지 어떻게 감사하면 좋을지에 대해서 가이딩 코디의 이야기를 들어볼게요.\n\n소중한 다른 사람을 돌봐주듯이 나 자신을 돌봐주며 지내보는거죠.\n\n오늘도 가이딩코디의 이야기를 들어볼게요:rabbit:\n\n:point_down: 오늘의 끼적이기 주제 보기\n https://resonance2heart.notion.site/1-1b3409abaef946b285c976389c1c89ac"

msg Ch4 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n이번주는 어떤 한주를 보내고 싶으신가요? 함께 원하는대로 만들어가봐요.\n\n먼저 2주차 이야기와 사용할 메모지 목록을 보내드려요.\n2주차 주제는 ”리틀코디에게 사랑주기” 주제로 진행합니다.\n\n오늘 월요일 주제는요,\n사랑해 메모지에 '내가 좋아하는 것' 끼적이기에요. 내가 좋아하는 것을 샅샅이 뒤져 메모지에 끼적이는거죠.\n내가 좋아하는 것을 알고 나에게 선물을 해주는 연습을 해볼거에요.\n\n:point_down: 오늘의 가이딩코디의 이야기 공유드립니다 :rabbit:\n https://resonance2heart.notion.site/2-a2c46a18d65b40e9bf2dc228ea265af8 \n\n그럼 오늘 하루도 행복하세요:orange_heart:"
msg Ch5 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 수요일 주제를 들고 왔는데요:heart: \n바로 '좋은 기억들 끼적여보기' 활동을 해볼거에요.\n다들 살아오면서 자신이 가진 좋은 기억들이 있을거에요. 많든 적든요. \n그 기억을 꺼내서 지금을 살아가는 원동력으로 만들어 볼게요.\n\n:point_down: 오늘의 가이딩코디의 이야기를 들어볼게요\n https://resonance2heart.notion.site/2-94627d16cdcc406685e22f3b8f28c2ff"
msg Ch6 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴입주민 여러분!\n\n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요:smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요,  바로 '자신의 장점을 끼적이기' 랍니다. \n\n:point_down: 오늘의 가이딩코디의 이야기를 들어볼게요\n https://resonance2heart.notion.site/2-d75f6c7a133d44a29730c75945a5069b"

msg Ch7 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n이번주는 어떤 한주를 보내고 싶으신가요? \n이번주는 확언을 통해 내가 만들어가는 삶을 더 가까이 당겨볼까요? \n\n먼저 3주차 이야기와 사용할 메모지 목록을 보내드려요. 3주차 주제는 \n”리틀코디와 깊은 대화하기” 라는 주제로 진행합니다.\n\n오늘 월요일 끼적이기는\n\n굴 메모지에 '나의 가득한 생각' 끼적이기에요. \n머리속에서 끊임없이 나를 괴롭히는 부정적인 생각이나 도움이 안되는 생각에 대해 써보는거죠.\n참고로 굴 메모지는 코디의 굴 입구에 위치한 굴 벽을 상징하기도 한답니다 :smiling_face_with_3_hearts: \n굴 입구 벽은 머리 속에 생각나는걸 휘갈겨 쓰는 공간이기도 해요.\n\n:point_down: 오늘의 가이딩코디의 이야기 공유드립니다:rabbit:\n https://resonance2heart.notion.site/3-4478e80edd134d91b85dcec004ef8f61 \n\n그럼 오늘 하루도 행복하세요:orange_heart:"
msg Ch8 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 수요일 주제를 들고 왔는데요!! \n바로 '느낌 적어보기' 활동을 해볼거에요.\n아니 왜 갑자기 무슨 느낌을 적냐고요? 이 느낌은 바로 \n\n**'우리가 월요일에 적었던 내 머리속을 가득채우고 있는 생각과 감정을 굴 벽에 끼적이기한 것에 대한 느낌'**을 적는걸 의미해요.\n\n자자 :point_down: 오늘의 가이딩코디의 이야기 공유드립니다:rabbit:\n https://resonance2heart.notion.site/3-e49d6e904f4942b0bf2be27e65ad4d88"
msg Ch9 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 금요일이에요. 평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 밤이 되길 바라요:smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요, \n바로 '나은 방향에 대한 생각 쓰기' 랍니다. \n\n이것 또한 앞에 **월요일, 수요일에 적으신 머리 속에 가득한 나의 생각과 감정에 대해 더 나은 방향으로 가려면 어떻게 해야할지에 대해 적어보는거에요.**\n\n:point_down: 오늘도 가이딩 코디의 이야기 들어볼게요.\n https://resonance2heart.notion.site/3-383eaa6bfce54d949771e3580925eda4"

msg Ch10 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n\n벌써 저희 4주차나 됐어요! 자신을 돌아보는데에 도움이 되시길 바라요!\n자 이번 한주는 어떻게 보내볼까요? 우리가 만들어보죠!\n\n이번 4주차 이야기와 사용할 메모지 목록을 보내드려요. \n4주차 주제는 ”리틀코디에게 용기주기(내면 토닥이기)” 라는 주제로 진행합니다.\n\n오늘 월요일 주제는요,\n할 수 있어! 포스트잇에 '두렵고 자신없는 것' 끼적이기에요. \n살면서 두려운게 없을 수는 없잖아요. 두렵지는 않아도 좀 걱정되는 것도 있고요.\n그런 것들을 써보는 시간이 되겠습니다:relaxed: \n\n:point_down: 자, 가이딩 코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/4-ac53fccf88284867b205270ebf932788"
msg Ch11 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 한 주의 중간같은 느낌인만큼 조금은 지치실텐데\n언제나 저희 토끼굴22번길은 여러분을 응원하고 격려드립니다아:rabbit:\n이번주 주제는 바로 '두려운 것에 대한 대응방안과 격려의 말 쓰기' 활동을 해볼거에요.\n지난 월,화요일에 두려운 것을 썼기에 이제 이 두려움을 내 일부로 수용하며 격려한 후,\n더 나은 삶을 살기 위한 대응방안을 써보는 시간을 갖는거죠.\n\n:point_down: 자자, 오늘의 가이딩코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/4-62d134fc0db0494bb9da929ab304ecf6"
msg Ch12 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 금요일이에요. 평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 밤이 되길 바라요:smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요, \n바로 '힘이 될 메세지 찾아서 적어보기' 입니다. \n이것 또한 지난 수요일에 적으신 격려의 말과 대응방안과 비슷하지만 이번에는 인터넷이나 다른 사람들의 말을 빌려 나에게 힘이 될 만한 메세지를 수집하고 내 마음 속에 내 것으로 가져오는 시간이 될거에요.\n\n:point_down: 오늘도 가이딩 코디의 이야기 들어볼게요.\n https://resonance2heart.notion.site/4-23010ca0ad7a4fb89e27e62d6c8075ba"

msg Ch13 = Text.pack "@everyone\n\n오늘은 월요일 한 주의 시작이에요:relaxed::two_hearts:\n한 주 동안도 함께 응원하면서 평온하게 지내보도록 해요!\n먼저 5주차 주제는 “인간관계' 입니다. 자신의 인간관계를 바라보고 진행할거에요.\n\n오늘 월요일 주제를 보고 함께 끼적여보아요.\n자기전에 올려주시면 됩니다:heart:\n\n https://resonance2heart.notion.site/5-29b95378a9f442f5bc406a4bd32a33f5"
msg Ch14 = Text.pack "@everyone\n\n오늘 하루도 평안하셨나요?:smiling_face_with_3_hearts: 드디어! 수요일 주제를 들고 왔는데요,\n바로 '나를 힘들게 한 사람에게 편지쓰기' 로 끼적여볼거에요.\n월요일 주제였던 나를 힘들게 한 사람들이나 특징에 대해 써봤는데 그 사람들에게\n하지 못했던 말을 해보는 시간인거죠.\n\n:point_down: 가이딩코디의 이야기 한번 들어보도록 할게요\n https://resonance2heart.notion.site/5-4801696e7c604e6696d28771e20c9548"
msg Ch15 = Text.pack "@everyone\n\n안녕하세요. 오늘은 금요일이에요. \n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요:smiling_face_with_3_hearts:\n오늘도 주제를 들고 왔는데요, \n바로 '불편한 사람에게 대응하는 방법 끼적이기' 랍니다. \n\n여태까지 불편한 사람을 떠올려보고 특징을 적으며 그 사람에게 편지쓰기 까지 했었는데요. \n이렇게 욕하고 나면 끝이었음 좋겠지만, 우리가 살아가야할 삶이 긴 만큼 비슷한 사람을 만나면 현명하게 대처하는 방법을 익혀두고 연습해야할 필요가 있어요. 그래야 같은 상처를 또다시 받지 않고 넘어갈 수 있을테니까요.\n\n:point_down: 자. 가이딩코디의 이야기를 들어볼게요.\nhttps://resonance2heart.notion.site/5-2e276321a5d14245b37b55402be08265"

msg Ch16 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n이번주는 아쉽게도 끼적이기 프로그램 마지막 주차에요.\n마지막인만큼 좀 더 열심히 참여해주시고 자신을 사랑하고 위한 노력\n앞으로도 계속 하기에요!\n\n먼저 6주차 이야기와 사용할 메모지 목록을 보내드려요. 6주차 주제는 \n”리틀코디가 그리는 미래” 라는 주제로 진행합니다.\n\n오늘 월요일 주제는요,\n따뜻한 하루 메모지 에 '버킷리스트 끼적이기' 에요. 앞으로 다가올 나는 무엇을 하고 싶은지\n자유롭게 적고 이루기 위해서는 어떻게 해야할지 적어보는거죠!\n\n:point_down: 자, 가이딩 코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/8-9f31325d955f4dd4ab6440beab7a8c2c"
msg Ch17 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 지치시겠지만 좀 더 마음을 가다듬고\n편안한 하루의 시간이 되시길 진심으로 바라요!\n\n이번주는 '거과의 기억 바꾸기' 활동을 해볼거에요.\n갑자기 과거의 기억을 바꾼다니. 이 뭔 SF 영화같은 말인가 하실텐데요!\n이게 실제로 가능합니다.\n\n오늘의 가이딩코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/8-387c40c187f54dc09c9ce7371847b4e8"
msg Ch18 = Text.pack "@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요:smiling_face_with_3_hearts:\n그리고 오늘 마지막 주제에요! >ㅁ<  뭔가 아쉽지만..그만큼 마지막을 멋지게 진행해보시죠!\n바로 '미래를 심상화 하기' 랍니다. 어엇 심상화라니, 좀 낯설고 어려워 보이시죠?  \n\n:point_down: 가이딩 코디의 이야기 들어볼게요.\n https://resonance2heart.notion.site/8-16427d2357634c478fc40975866f4971"


-- :rabbit:    :point_down:    :heart:  :two_hearts:   :smiling_face_with_3_hearts:   :relaxed:   

-- (day, (msg, [img]))
-- programMessage :: [(Text, (Text, [Text]))] 
-- programMessage = zip ts textMsg
--     where
--          textMsg = zip programMsg imgs


imgUrl :: Int -> Text -> Text
imgUrl week name = "https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/week" <> (Text.pack . show) week <> "/" <> name

ch1Imgs :: [Text]
ch1Imgs = [ imgUrl 1 "mon-1.jpg"
          , imgUrl 1 "mon-2.jpg"
          , imgUrl 1 "mon-3.jpg"
          , imgUrl 1 "mon-4.jpg"
          , imgUrl 1 "mon-5.jpg"
          ]

ch2Imgs :: [Text]
ch2Imgs = [ imgUrl 1 "wed-1.jpg"
          , imgUrl 1 "wed-2.jpg" 
          ]

ch3Imgs :: [Text]
ch3Imgs = [ imgUrl 1 "fri-1.jpg"
          , imgUrl 1 "fri-2.jpg" 
          , imgUrl 1 "fri-3.jpg" 
          ]

ch4Imgs :: [Text]
ch4Imgs = [ imgUrl 2 "mon-1.jpg"
          , imgUrl 2 "mon-2.jpg"
          , imgUrl 2 "mon-3.jpg"
          , imgUrl 2 "mon-4.jpg"
          , imgUrl 2 "mon-5.jpg"
          ]

ch5Imgs :: [Text]
ch5Imgs = [ imgUrl 2 "wed-1.jpg"
          , imgUrl 2 "wed-2.jpg" 
          ]

ch6Imgs :: [Text]
ch6Imgs = [ imgUrl 2 "fri-1.jpg"
          , imgUrl 2 "fri-2.jpg" 
          ]

ch7Imgs :: [Text]
ch7Imgs = [ imgUrl 3 "mon-1.jpg"
          , imgUrl 3 "mon-2.jpg"
          , imgUrl 3 "mon-3.jpg"
          , imgUrl 3 "mon-4.jpg"
          , imgUrl 3 "mon-5.jpg"
          ]

ch8Imgs :: [Text]
ch8Imgs = [ imgUrl 3 "wed-1.jpg"
          , imgUrl 3 "wed-2.jpg" 
          ]

ch9Imgs :: [Text]
ch9Imgs = [ imgUrl 3 "fri-1.jpg"
          , imgUrl 3 "fri-2.jpg" 
          ]

ch10Imgs :: [Text]
ch10Imgs = [ imgUrl 4 "mon-1.jpg"
          , imgUrl 4 "mon-2.jpg"
          , imgUrl 4 "mon-3.jpg"
          , imgUrl 4 "mon-4.jpg"
          , imgUrl 4 "mon-5.jpg"
          ]

ch11Imgs :: [Text]
ch11Imgs = [ imgUrl 4 "wed-1.jpg"
          , imgUrl 4 "wed-2.jpg" 
          ]

ch12Imgs :: [Text]
ch12Imgs = [ imgUrl 4 "fri-1.jpg"
          , imgUrl 4 "fri-2.jpg" 
          ]

ch13Imgs :: [Text]
ch13Imgs = [ imgUrl 5 "mon-1.jpg"
          , imgUrl 5 "mon-2.jpg"
          , imgUrl 5 "mon-3.jpg"
          , imgUrl 5 "mon-4.jpg"
          , imgUrl 5 "mon-5.jpg"
          ]

ch14Imgs :: [Text]
ch14Imgs = [ imgUrl 5 "wed-1.jpg"
          , imgUrl 5 "wed-2.jpg" 
          ]

ch15Imgs :: [Text]
ch15Imgs = [ imgUrl 5 "fri-1.jpg"
          , imgUrl 5 "fri-2.jpg" 
          ]

ch16Imgs :: [Text]
ch16Imgs = [ imgUrl 6 "mon-1.jpg"
          , imgUrl 6 "mon-2.jpg"
          , imgUrl 6 "mon-3.jpg"
          , imgUrl 6 "mon-4.jpg"
          , imgUrl 6 "mon-5.jpg"
          ]

ch17Imgs :: [Text]
ch17Imgs = [ imgUrl 6 "wed-1.jpg"
          , imgUrl 6 "wed-2.jpg" 
          ]

ch18Imgs :: [Text]
ch18Imgs = [ imgUrl 6 "fri-1.jpg"
          , imgUrl 6 "fri-2.jpg" 
          ]



img :: Program22st3Chapters -> [Text]
img Ch1 = ch1Imgs
img Ch2 = ch2Imgs
img Ch3 = ch3Imgs

img Ch4 = ch4Imgs
img Ch5 = ch5Imgs
img Ch6 = ch6Imgs

img Ch7 = ch7Imgs
img Ch8 = ch8Imgs
img Ch9 = ch9Imgs

img Ch10 = ch10Imgs
img Ch11 = ch11Imgs
img Ch12 = ch12Imgs

img Ch13 = ch13Imgs
img Ch14 = ch14Imgs
img Ch15 = ch15Imgs

img Ch16 = ch16Imgs
img Ch17 = ch17Imgs
img Ch18 = ch18Imgs

-- 
