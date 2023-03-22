{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module DateRangeTest where 

import Data.Aeson
import GHC.Generics
-- import Network.HTTP.Conduit (simpleHttp)
import Data.Text (Text, intercalate)
-- import Data.Time
-- import Data.Dates 
import Data.List
import Data.DateTime as DT 
import Data.Time.Clock  hiding (getCurrentTime)
import Data.Time.Calendar as Cal hiding (fromGregorian)
import Data.Time.Calendar.WeekDate as CalWee
import Data.Time.LocalTime
-- import Data.Map (Map, fromList, toList, insert, lookup)
import qualified Data.ByteString.Lazy as BSL hiding (lenght)
import qualified Data.ByteString.Internal as BSI
import Control.Applicative
import MaybeFunctionTest

-- Data.DateTime.DateTime (DDT)
-- toSeconds :: DateTime -> Integer
-- fromSeconds :: Integer -> DateTime
-- toClockTime :: DateTime -> ClockTimeSource
-- fromClockTime :: ClockTime -> DateTime

-- date' <- getCurrentTime



type ParsedDate = (Integer, Int, Int, Int, Int, Int)




data Slot = Slot Int (LocalTime, LocalTime)
    deriving (Show, Eq)


cmpId :: (Int, a) -> (Int, a) -> Bool 
cmpId (id1, _) (id2, _) = id1 == id2


firstDate :: DateTime
firstDate = DT.fromGregorian 2022 8 1 4 0 0

nextDay :: DateTime
nextDay = addMinutes 1440 firstDate

lastDate :: DateTime
lastDate = DT.fromGregorian 2022 8 30 4 0 0

dateTupl :: ParsedDate
dateTupl = DT.toGregorian firstDate

-- {"name":{"S":"Baaaam"},"attendance":{"SS":["2022-07-30 05:02:37.294456777 UTC","2022-07-30 05:03:02.85413083 UTC","2022-07-30 06:01:33.044321211 UTC","2022-07-30 06:01:42.527053904 UTC","2022-07-31 09:31:32.808555769 UTC","2022-07-31 09:48:43.374597613 UTC","2022-07-31 09:52:49.875613045 UTC","2022-07-31 15:05:53.166820986 UTC","2022-07-31 15:30:52.39299854 UTC","2022-07-31 15:42:39.368028029 UTC","2022-07-31 15:43:55.289264423 UTC","2022-07-31 15:52:53.474517794 UTC","2022-07-31 15:53:18.015164289 UTC","2022-07-31 16:15:34.725638702 UTC","2022-07-31 16:16:11.186776959 UTC","2022-07-31 16:17:45.028253638 UTC","2022-08-01 07:56:15.917088476 UTC","2022-08-01 07:56:24.675440327 UTC","2022-08-01 07:56:31.836065992 UTC","2022-08-01 08:23:59.507437622 UTC","2022-08-01 08:24:08.087265217 UTC","2022-08-01 08:24:15.066802079 UTC","2022-08-01 08:29:07.011312257 UTC","2022-08-01 08:32:24.674631906 UTC","2022-08-01 09:38:44.314758315 UTC","2022-08-01 09:42:35.561698133 UTC","2022-08-01 14:52:02.066411024 UTC","2022-08-01 15:50:54.464293593 UTC","2022-08-08 15:16:26.520908099 UTC"]}}
attendance = ["2022-08-30 05:02:37.294456777 UTC","2022-08-30 05:03:02.85413083 UTC","2022-08-30 06:01:33.044321211 UTC","2022-08-30 06:01:42.527053904 UTC","2022-09-01 09:31:32.808555769 UTC","2022-09-01 09:48:43.374597613 UTC","2022-09-03 09:52:49.875613045 UTC","2022-09-04 15:05:53.166820986 UTC","2022-09-05 15:30:52.39299854 UTC","2022-09-06 15:42:39.368028029 UTC","2022-09-06 15:43:55.289264423 UTC","2022-09-06 15:52:53.494519994 UTC","2022-09-09 15:53:18.015164289 UTC","2022-09-13 16:15:34.925638702 UTC","2022-09-14 16:16:11.186776959 UTC","2022-09-20 16:17:45.028253638 UTC","2022-09-22 07:56:15.917088476 UTC","2022-09-22 07:56:24.675440327 UTC"]

attendanceToSeconds :: [Integer]
attendanceToSeconds = map (\x -> (toSeconds $ read x)) attendance

time :: IO Integer
time = do 
    ct <- getCurrentTime
    return $ toSeconds ct


test :: IO ()
test = putStrLn ((show firstCheck) ++ "일을 기준으로 총 출석일은 " ++ (show totalCheck) ++ " 일 입니다.")
    where 
        checked = filter (\(i, (Slot x (y,y'))) -> x==1 ) (check 1661868166 attendanceToSeconds)
        totalCheck = length (groupBy cmpId checked)
        -- totalCheck = length (map head checkedGroup)


        (id, Slot i (d1,d2)) = head (map head (groupBy cmpId checked))
        firstCheck = localDay d1

    


-- 1661835757
-- filter (\(x,(y,y')) -> (x == 1)) (check 1661835757)
check ::Integer -> [Integer] -> [(Int, Slot)]
check time userDates = interval dayCheck localList datesAddId
    -- filter checkTest checkList
    where
        localList = map (\x -> (utcToLocalTime (hoursToTimeZone 9) (fromSeconds x))) userDates
        dates = createDates time
        datesAddId = zip [0 .. (length dates)] dates
    -- map (\x -> (dayCheck localList x)) $ createDates time
    -- (,) <$> createDates sec <*> attendanceToSeconds 
    -- return checkList

-- intervals :: [Slot]
-- intervals = do
--     -- filter checkTest checkList
--     let localList = map (\x -> (utcToLocalTime (hoursToTimeZone 9) (fromSeconds 1661835757))) attendanceToSeconds

--     map (\x -> (dayCheck localList x)) checkList

-- today :: LocalTime
-- today = utcToLocalTime (hoursToTimeZone 9) $ fromGregorian 2022 8 30 18 30 0

interval :: (LocalTime -> (Int, Slot) -> (Int, Slot)) -> [LocalTime] -> [(Int, Slot)] -> [(Int, Slot)]
interval f attendances slotes = f <$> attendances <*> slotes

dayCheck :: LocalTime -> (Int, Slot) -> (Int, Slot)
-- dayCheck time (x,(y,y')) 
dayCheck time (id, (Slot x (y, y')))
    | (x == 1) = (id, (Slot x (y,y')))
    | (y < time) && (y' > time) = (id, (Slot 1 (y, y')))
    | otherwise = (id, (Slot 0 (y, y')))

-- dayCheck :: [LocalTime] -> Slot -> Slot
-- -- dayCheck time (x,(y,y')) 
-- dayCheck time (Slot x (y, y'))
--     | (x == 1) = Slot x (y,y')
--     | (y < time) && (y' > time) = Slot 1 (y, y')
--     | otherwise = Slot 0 (y, y')


-- dayCheck :: Slot -> Statement
-- dayCheck (x,(y,y'))
--     | (y < today) && (y' > today) && (x==0)= 1
--     | (y < today) && (y' > today) && (x==1)= 2
--     | otherwise = (0,(y,y'))
       
-- checkTest ((x,(y,y')):_) 
--     | (x == 0) = print "True"
-- checkTest ((x,(y,y')):ds) 
--     | (x == 0) = checkTest ds
        


algebMonad :: Int -> Int
algebMonad = do
    x <- (+2)
    y <- (*3)
    return (x + y)





createDates :: Integer -> [Slot]
createDates time = attendanceTuple localList
    where
        localDate = (utcToLocalTime (hoursToTimeZone 9) (fromSeconds time))
        day@(y, m, d) = toWeekDate $ localDay localDate
        gregCal = [(CalWee.fromWeekDate y m d) .. (CalWee.fromWeekDate y (m+6) d)]
        localList = map (\ld -> LocalTime { localDay = ld, localTimeOfDay = TimeOfDay { todHour = 4, todMin = 0, todSec = 0}}) gregCal


-- createDates :: IO [(Int, (DateTime, DateTime))]
-- createDates :: IO [(Integer, Int, Int)]
-- createDates :: IO (Integer, Int, Int)
-- createDates :: IO [DateTime]
-- createDates :: IO [TimeOfDay]
-- createDates :: IO [LocalTime]
-- createDates :: IO LocalTime
-- createDates :: IO CalendarDiffTime
    -- ct <- getCurrentTime
    -- let localDate = (utcToLocalTime (hoursToTimeZone 9) (fromSeconds time))
    -- let rt = daysAndTimeOfDayToTime 0 $ localTimeOfDay localDate
    -- let at = daysAndTimeOfDayToTime 0 $ TimeOfDay { todHour = 4, todMin = 0, todSec = 0 }
    -- return $ addLocalTime (at - rt) localDate

    -- let day@(y, m, d) = toWeekDate $ localDay localDate
    -- let gregCal = [(CalWee.fromWeekDate y m d) .. (CalWee.fromWeekDate y (m+6) d)]
    -- let fds = take 5 [localDay localDate .. ]
    -- let localList = map (\ld -> LocalTime { localDay = ld, localTimeOfDay = TimeOfDay { todHour = 4, todMin = 0, todSec = 0}}) gregCal
    
    -- let day@(y, m, d) = toWeekDate (localDay (utcToLocalTime (hoursToTimeZone 9) ct))
    -- let gregCal = map Cal.toGregorian [(CalWee.fromWeekDate y m d) .. (CalWee.fromWeekDate y (m+6) d)]

    -- let gregCal = map Cal.toGregorian $ take 5 [ (localDay (utcToLocalTime (hoursToTimeZone 9) ct)) ..]
    -- let gregCal = map Cal.toGregorian $ take 5 [Cal.fromGregorian 2022 8 1..]
    -- let gregCal = map Cal.toGregorian $ [CalWee.fromWeekDate 2022 30 7 .. CalWee.fromWeekDate 2022 36 7]
    -- return gregCal

    -- let timeOfDay = map $ take 5 [(localTimeOfDay (utcToLocalTime (hoursToTimeZone 9) ct)) ..]
    -- Just loocalHour <- makeTimeOfDayValid 4 0 0
    -- return timeOfDay

    -- return $ map (\(y, m, d) -> DT.fromGregorian y m d 4 0 0) gregCal

    -- let gregUtc = map (\(y, m, d) -> DT.fromGregorian y m d 4 0 0) gregCal
    -- return $ map (\x -> (utcToLocalTime (hoursToTimeZone 0) x)) gregUtc

    -- let gregUtc = map (\(y, m, d) -> DT.fromGregorian y m d 19 0 0) gregCal
    -- return gregUtc
    -- return $ attendanceTuple gregUtc

    -- let fromLocal = map (\x -> (utcToLocalTime (hoursToTimeZone 9) x)) gregUtc
    -- return fromLocal

    

attendanceTuple :: [LocalTime] -> [Slot]
attendanceTuple dates = map (\(d1, d2) -> Slot 0 (d1, d2) ) datesTuple
    where 
        datesTuple = zip dates (tail dates)

-- a :: (Num a) => [a]
-- a = [1,2,3,4,5,6,7,8]
-- tupleTest :: [Int] -> [(Int, Int)]
-- tupleTest (x:y:_) = [(x,y)]
-- tupleTest (x:y:is) = tupleTest ((x,y) : is)



monadicComb :: [Slot] -> [(Slot, String)]
monadicComb intervals = do
    inter <- intervals 
    currt <- attendance
    return $ (inter, currt)


-- (2022,8,22,14,22,35) 6
getDay :: ParsedDate -> Int
getDay (_,_,x,_,_,_) = x

getHour :: ParsedDate -> Int
getHour (_,_,_,x,_,_) = x

-- isSaturday :: Day -> Bool
-- isSaturday d = weekday d == 6
--   where
--     weekday :: Day -> Int
--     weekday = sel3 . toWeekDate

-- rangeWithoutSaturdays :: Day -> Day -> [Day]
-- rangeWithoutSaturdays start end = filter (not . isSaturday) [start .. end]

-- rangeWithoutSaturdays :: DateTime -> DateTime -> [DateTime]
-- rangeWithoutSaturdays start end = filter timestamp) dateList
--     where
--         nextDay = addMinutes 1440 start
--         dateList = [(toSeconds start),(toSeconds nextDay)  .. (toSeconds end)]


-- fromWeekDate 2024 31 4 // 년도, 1주~53주 1~7요일 요일이 안 맞으면 비슷한 숫자로 맞게 적용 // 2024-08-01



-- test :: Int
-- test = do
--     let x = 10  
--     let y = 20 
--     x + y





-- {
--     "program_locale":{"S":"en-US"},
--     "operator_name":{"S":"Baaaam"},
--     "program_timezone":{"N":"9"},
--     "program_thread_id":{"S":"945510596093292564"},
--     "operator_id":{"S":"602853327784640532"},
--     "members":{"L":[
--         {"M":{"name":{"S":"leechanwoo"},"id":{"S":"687326243703881793"}}},
--         {"M":{"name":{"S":"Baaaam"},"id":{"S":"602853327784640532"}}}
--         ]},
--     "program_name":{"S":"little of wrtie 22st1"},
--     "program_start_date":{"N":"1661868166"},
--     "limits":{"N":"0"}
-- }

-- {
--     "resource": "/guiding-codey/scheduled/lunch-menu",
--     "path": "/guiding-codey/scheduled/lunch-menu/",
--     "httpMethod": "GET",
--     "headers": {
--         "amz-sdk-invocation-id": "61e5d6df-5def-0049-0f59-78bcbdcf1af9",
--         "amz-sdk-request": "ttl=20220902T030107Z;attempt=1;max=4",
--         "amz-sdk-retry": "0/0/500",
--         "Content-Type": "",
--         "Host": "cocucga4ra.execute-api.ap-northeast-2.amazonaws.com",
--         "User-Agent": "aws-sdk-java/1.12.277 Linux/5.4.204-124.362.amzn2int.x86_64 OpenJDK_64-Bit_Server_VM/25.342-b07 java/1.8.0_342 vendor/Oracle_Corporation cfg/retry-mode/legacy",
--         "X-Amz-Date": "20220902T030017Z",
--         "X-Amz-Source-Account": "901019266308",
--         "X-Amz-Source-Arn": "arn:aws:events:ap-northeast-2:901019266308:rule/guiding-codey-lunch-menu",
--         "X-Amzn-Trace-Id": "Root=1-631171c1-6cc5192139b83d4c60c02cb9",
--         "X-Forwarded-For": "54.239.116.70",
--         "X-Forwarded-Port": "443",
--         "X-Forwarded-Proto": "https"
--     },
--     "multiValueHeaders": {
--         "amz-sdk-invocation-id": [
--             "61e5d6df-5def-0049-0f59-78bcbdcf1af9"
--         ],
--         "amz-sdk-request": [
--             "ttl=20220902T030107Z;attempt=1;max=4"
--         ],
--         "amz-sdk-retry": [
--             "0/0/500"
--         ],
--         "Content-Type": [
--             ""
--         ],
--         "Host": [
--             "cocucga4ra.execute-api.ap-northeast-2.amazonaws.com"
--         ],
--         "User-Agent": [
--             "aws-sdk-java/1.12.277 Linux/5.4.204-124.362.amzn2int.x86_64 OpenJDK_64-Bit_Server_VM/25.342-b07 java/1.8.0_342 vendor/Oracle_Corporation cfg/retry-mode/legacy"
--         ],
--         "X-Amz-Date": [
--             "20220902T030017Z"
--         ],
--         "X-Amz-Source-Account": [
--             "901019266308"
--         ],
--         "X-Amz-Source-Arn": [
--             "arn:aws:events:ap-northeast-2:901019266308:rule/guiding-codey-lunch-menu"
--         ],
--         "X-Amzn-Trace-Id": [
--             "Root=1-631171c1-6cc5192139b83d4c60c02cb9"
--         ],
--         "X-Forwarded-For": [
--             "54.239.116.70"
--         ],
--         "X-Forwarded-Port": [
--             "443"
--         ],
--         "X-Forwarded-Proto": [
--             "https"
--         ]
--     },
--     "queryStringParameters": null,
--     "multiValueQueryStringParameters": null,
--     "pathParameters": null,
--     "stageVariables": null,

--     "requestContext": {
--         "resourceId": "44hvy8",
--         "resourcePath": "/guiding-codey/scheduled/lunch-menu",
--         "httpMethod": "GET",
--         "extendedRequestId": "Xz62PEujIE0Fk8A=",
--         "requestTime": "02/Sep/2022:03:00:17 +0000",
--         "path": "/guiding-codey-release/guiding-codey/scheduled/lunch-menu/",
--         "accountId": "901019266308",
--         "protocol": "HTTP/1.1",
--         "stage": "guiding-codey-release",
--         "domainPrefix": "cocucga4ra",

--         "requestTimeEpoch": 1662087617398,

--         "requestId": "ca5be45a-9bb4-4f77-8366-8a7802a1eaa5",
--         "identity": {
--             "cognitoIdentityPoolId": null,
--             "accountId": null,
--             "cognitoIdentityId": null,
--             "caller": null,
--             "sourceIp": "54.239.116.70",
--             "principalOrgId": null,
--             "accessKey": null,
--             "cognitoAuthenticationType": null,
--             "cognitoAuthenticationProvider": null,
--             "userArn": null,
--             "userAgent": "aws-sdk-java/1.12.277 Linux/5.4.204-124.362.amzn2int.x86_64 OpenJDK_64-Bit_Server_VM/25.342-b07 java/1.8.0_342 vendor/Oracle_Corporation cfg/retry-mode/legacy",
--             "user": null
--         },
--         "domainName": "cocucga4ra.execute-api.ap-northeast-2.amazonaws.com",
--         "apiId": "cocucga4ra"
--     },

--     "body": null,
--     "isBase64Encoded": false
-- }

-- {"message":{
--     "M":{
--     "1662253200":{"SS":["@everyone \n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n한 주 동안도 함께 응원하고 격려하며 평온하게 지내보도록 해요 :open_hands:\n\n먼저 1주차 이야기와 1주차에 사용할 메모지 목록을 보내드려요.\n매주가 시작되는 월요일에는 그 주에 진행할 이야기와 메모지 목록을 보내드린답니다 :smiling_face_with_3_hearts:\n\n그리고 월요일 주제도 함께 나눠볼게요! 잘 읽고 오늘밤 주무시기 전에 끼적인거 인증해주심 되어요오오오 :rabbit:\n그럼 오늘 하루도 행복하세요:orange_heart:\n\n오늘의 가이딩코디의 이야기 공유드립니다 :rabbit: \n\n:point_down: 오늘의 끼적이기 주제 보기 \n https://resonance2heart.notion.site/1-e88afc5dbde7472ea9865197dcd861c4"]},
--     "1662512400":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 지칠 수 있는 수요일이지만 무리하지말고 조금은 쉬어가는 그런 날이 되셨으면 좋겠어요! \n자 오늘도 수요일 주제를 들고 왔는데요, \n바로 '나 자신을 관찰하고 끼적이기' 로 활동을 해볼거에요.\n소중한 다른 사람을 돌봐주듯이 나 자신을 돌봐주며 지내보는거죠.\n\n오늘의 가이딩코디의 이야기를 들어볼게요:rabbit:\n\n:point_down: 오늘의 끼적이기 주제 보기\n https://resonance2heart.notion.site/1-e02eb481dbbf4f8da11a159291afcb95"]},
--     "1662771600":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요 :smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요, \n바로 '감사하기' 랍니다. 이건 너무나 익숙한 주제이기도 하죠?\n\n감사하기가 좋다는건 누구나 다 아는 사실일텐데요. 정확히 어떻게 좋은지 어떻게 감사하면 좋을지에 대해서 가이딩 코디의 이야기를 들어볼게요.\n\n소중한 다른 사람을 돌봐주듯이 나 자신을 돌봐주며 지내보는거죠.\n\n오늘도 가이딩코디의 이야기를 들어볼게요:rabbit:\n\n:point_down: 오늘의 끼적이기 주제 보기\n https://resonance2heart.notion.site/1-1b3409abaef946b285c976389c1c89ac"]},
    
--     "1662944400":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n이번주는 어떤 한주를 보내고 싶으신가요? 함께 원하는대로 만들어가봐요.\n\n먼저 2주차 이야기와 사용할 메모지 목록을 보내드려요.\n2주차 주제는 ”리틀코디에게 사랑주기” 주제로 진행합니다.\n\n오늘 월요일 주제는요,\n사랑해 메모지에 '내가 좋아하는 것' 끼적이기에요. 내가 좋아하는 것을 샅샅이 뒤져 메모지에 끼적이는거죠.\n내가 좋아하는 것을 알고 나에게 선물을 해주는 연습을 해볼거에요.\n\n:point_down: 오늘의 가이딩코디의 이야기 공유드립니다 :rabbit:\n https://resonance2heart.notion.site/2-a2c46a18d65b40e9bf2dc228ea265af8 \n\n그럼 오늘 하루도 행복하세요:orange_heart:"]},
--     "1663117200":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 수요일 주제를 들고 왔는데요:heart: \n바로 '좋은 기억들 끼적여보기' 활동을 해볼거에요.\n다들 살아오면서 자신이 가진 좋은 기억들이 있을거에요. 많든 적든요. \n그 기억을 꺼내서 지금을 살아가는 원동력으로 만들어 볼게요.\n\n:point_down: 오늘의 가이딩코디의 이야기를 들어볼게요\n https://resonance2heart.notion.site/2-94627d16cdcc406685e22f3b8f28c2ff"]},
--     "1663290000":{"SS":["@everyone\n\n안녕하세요~!  토끼굴입주민 여러분!\n\n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요:smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요,  바로 '자신의 장점을 끼적이기' 랍니다. \n\n:point_down: 오늘의 가이딩코디의 이야기를 들어볼게요\n https://resonance2heart.notion.site/2-d75f6c7a133d44a29730c75945a5069b"]},
    
--     "1663549200":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n이번주는 어떤 한주를 보내고 싶으신가요? \n이번주는 확언을 통해 내가 만들어가는 삶을 더 가까이 당겨볼까요? \n\n먼저 3주차 이야기와 사용할 메모지 목록을 보내드려요. 3주차 주제는 \n”리틀코디와 깊은 대화하기” 라는 주제로 진행합니다.\n\n오늘 월요일 끼적이기는\n\n굴 메모지에 '나의 가득한 생각' 끼적이기에요. \n머리속에서 끊임없이 나를 괴롭히는 부정적인 생각이나 도움이 안되는 생각에 대해 써보는거죠.\n참고로 굴 메모지는 코디의 굴 입구에 위치한 굴 벽을 상징하기도 한답니다 :smiling_face_with_3_hearts: \n굴 입구 벽은 머리 속에 생각나는걸 휘갈겨 쓰는 공간이기도 해요.\n\n:point_down: 오늘의 가이딩코디의 이야기 공유드립니다:rabbit:\n https://resonance2heart.notion.site/3-4478e80edd134d91b85dcec004ef8f61\n\n그럼 오늘 하루도 행복하세요:orange_heart:"]},
--     "1663722000":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 수요일 주제를 들고 왔는데요!! \n바로 '느낌 적어보기' 활동을 해볼거에요.\n아니 왜 갑자기 무슨 느낌을 적냐고요? 이 느낌은 바로 \n\n**'우리가 월요일에 적었던 내 머리속을 가득채우고 있는 생각과 감정을 굴 벽에 끼적이기한 것에 대한 느낌'**을 적는걸 의미해요.\n\n자자 :point_down: 오늘의 가이딩코디의 이야기 공유드립니다:rabbit:\n https://resonance2heart.notion.site/3-e49d6e904f4942b0bf2be27e65ad4d88"]},
--     "1663894800":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 금요일이에요. 평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 밤이 되길 바라요:smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요, \n바로 '나은 방향에 대한 생각 쓰기' 랍니다. \n\n이것 또한 앞에 **월요일, 수요일에 적으신 머리 속에 가득한 나의 생각과 감정에 대해 더 나은 방향으로 가려면 어떻게 해야할지에 대해 적어보는거에요.**\n\n:point_down: 오늘도 가이딩 코디의 이야기 들어볼게요.\n https://resonance2heart.notion.site/3-383eaa6bfce54d949771e3580925eda4"]},
    
--     "1664154000":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n\n벌써 저희 4주차나 됐어요! 자신을 돌아보는데에 도움이 되시길 바라요!\n자 이번 한주는 어떻게 보내볼까요? 우리가 만들어보죠!\n\n이번 4주차 이야기와 사용할 메모지 목록을 보내드려요. \n4주차 주제는 ”리틀코디에게 용기주기(내면 토닥이기)” 라는 주제로 진행합니다.\n\n오늘 월요일 주제는요,\n할 수 있어! 포스트잇에 '두렵고 자신없는 것' 끼적이기에요. \n살면서 두려운게 없을 수는 없잖아요. 두렵지는 않아도 좀 걱정되는 것도 있고요.\n그런 것들을 써보는 시간이 되겠습니다:relaxed: \n\n:point_down: 자, 가이딩 코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/4-ac53fccf88284867b205270ebf932788"]},
--     "1664326800":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 한 주의 중간같은 느낌인만큼 조금은 지치실텐데\n언제나 저희 토끼굴22번길은 여러분을 응원하고 격려드립니다아:rabbit:\n이번주 주제는 바로 '두려운 것에 대한 대응방안과 격려의 말 쓰기' 활동을 해볼거에요.\n지난 월,화요일에 두려운 것을 썼기에 이제 이 두려움을 내 일부로 수용하며 격려한 후,\n더 나은 삶을 살기 위한 대응방안을 써보는 시간을 갖는거죠.\n\n:point_down: 자자, 오늘의 가이딩코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/4-62d134fc0db0494bb9da929ab304ecf6"]},
--     "1664499600":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 금요일이에요. 평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 밤이 되길 바라요:smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요, \n바로 '힘이 될 메세지 찾아서 적어보기' 입니다. \n이것 또한 지난 수요일에 적으신 격려의 말과 대응방안과 비슷하지만 이번에는 인터넷이나 다른 사람들의 말을 빌려 나에게 힘이 될 만한 메세지를 수집하고 내 마음 속에 내 것으로 가져오는 시간이 될거에요.\n\n:point_down: 오늘도 가이딩 코디의 이야기 들어볼게요.\n https://resonance2heart.notion.site/4-23010ca0ad7a4fb89e27e62d6c8075ba"]},
    
--     "1664758800":{"SS":["@everyone\n\n오늘은 월요일 한 주의 시작이에요:relaxed::two_hearts:\n한 주 동안도 함께 응원하면서 평온하게 지내보도록 해요!\n먼저 5주차 주제는 “인간관계' 입니다. 자신의 인간관계를 바라보고 진행할거에요.\n\n오늘 월요일 주제를 보고 함께 끼적여보아요.\n자기전에 올려주시면 됩니다:heart:\n\n https://resonance2heart.notion.site/5-29b95378a9f442f5bc406a4bd32a33f5"]},
--     "1664931600":{"SS":["@everyone\n\n오늘 하루도 평안하셨나요?:smiling_face_with_3_hearts: 드디어! 수요일 주제를 들고 왔는데요,\n바로 '나를 힘들게 한 사람에게 편지쓰기' 로 끼적여볼거에요.\n월요일 주제였던 나를 힘들게 한 사람들이나 특징에 대해 써봤는데 그 사람들에게\n하지 못했던 말을 해보는 시간인거죠.\n\n:point_down: 가이딩코디의 이야기 한번 들어보도록 할게요\n https://resonance2heart.notion.site/5-4801696e7c604e6696d28771e20c9548"]},
--     "1665104400":{"SS":["@everyone\n\n안녕하세요. 오늘은 금요일이에요. \n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요:smiling_face_with_3_hearts:\n오늘도 주제를 들고 왔는데요, \n바로 '불편한 사람에게 대응하는 방법 끼적이기' 랍니다. \n\n여태까지 불편한 사람을 떠올려보고 특징을 적으며 그 사람에게 편지쓰기 까지 했었는데요. \n이렇게 욕하고 나면 끝이었음 좋겠지만, 우리가 살아가야할 삶이 긴 만큼 비슷한 사람을 만나면 현명하게 대처하는 방법을 익혀두고 연습해야할 필요가 있어요. 그래야 같은 상처를 또다시 받지 않고 넘어갈 수 있을테니까요.\n\n:point_down: 자. 가이딩코디의 이야기를 들어볼게요.\nhttps://resonance2heart.notion.site/5-2e276321a5d14245b37b55402be08265"]},
    
--     "1665363600":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n오늘은 월요일 한 주의 시작이에요:tada:\n이번주는 아쉽게도 끼적이기 프로그램 마지막 주차에요.\n마지막인만큼 좀 더 열심히 참여해주시고 자신을 사랑하고 위한 노력\n앞으로도 계속 하기에요!\n\n먼저 6주차 이야기와 사용할 메모지 목록을 보내드려요. 6주차 주제는 \n”리틀코디가 그리는 미래” 라는 주제로 진행합니다.\n\n오늘 월요일 주제는요,\n따뜻한 하루 메모지 에 '버킷리스트 끼적이기' 에요. 앞으로 다가올 나는 무엇을 하고 싶은지\n자유롭게 적고 이루기 위해서는 어떻게 해야할지 적어보는거죠!\n\n:point_down: 자, 가이딩 코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/8-9f31325d955f4dd4ab6440beab7a8c2c"]},
--     "1665536400":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n오늘은 수요일이에요. 지치시겠지만 좀 더 마음을 가다듬고\n편안한 하루의 시간이 되시길 진심으로 바라요!\n\n이번주는 '거과의 기억 바꾸기' 활동을 해볼거에요.\n갑자기 과거의 기억을 바꾼다니. 이 뭔 SF 영화같은 말인가 하실텐데요!\n이게 실제로 가능합니다.\n\n오늘의 가이딩코디의 이야기를 들어볼게요.\n https://resonance2heart.notion.site/8-387c40c187f54dc09c9ce7371847b4e8"]},
--     "1665709200":{"SS":["@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요:smiling_face_with_3_hearts:\n그리고 오늘 마지막 주제에요! >ㅁ<  뭔가 아쉽지만..그만큼 마지막을 멋지게 진행해보시죠!\n바로 '미래를 심상화 하기' 랍니다. 어엇 심상화라니, 좀 낯설고 어려워 보이시죠?  \n\n:point_down: 가이딩 코디의 이야기 들어볼게요.\n https://resonance2heart.notion.site/8-16427d2357634c478fc40975866f4971"]}}
-- },
-- "name":{"S":"22st3"},
-- "images":
--     {"M":{
--     "1662253200":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg"]},
--     "1662512400":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]},
--     "1662771600":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-3.jpg"]},
    
--     "1662944400":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg"]},
--     "1663117200":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]},
--     "1663290000":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/2%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg"]},
    
--     "1663549200":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg"]},
--     "1663722000":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]},
--     "1663894800":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/3%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg"]},
    
--     "1664154000":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg"]},
--     "1664326800":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]},
--     "1664499600":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/4%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg"]},
    
--     "1664758800":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg"]},
--     "1664931600":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]},
--     "1665104400":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/5%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg"]},
    
--     "1665363600":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/mon-M3.jpg"]},
--     "1665536400":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/wed-2.jpg"]},
--     "1665709200":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/6%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg"]}}}
-- }

-- [(ts, [Text]), (ts, [Text]), (ts, [Text]), (ts, [Text])]

-- {
--     "timestamp" : Integer
--     , "images" : [URL]
-- },
-- {
--     "timestamp" : Integer
--     , "images" : [URL]
-- },
-- {
--     "timestamp" : Integer
--     , "images" : [URL]
-- },

-- images : [ [ URL ] ]
-- message : [ Text ]
-- timestamp : [ timestamp ]

-- {
--     "message":{"S":"@everyone\n\n안녕하세요~!  토끼굴 입주민 여러분!\n\n평일 5일 중 마지막 날인만큼 편안하게 업무와 공부를 마무리 하시는 날이 되길 바라요 :smiling_face_with_3_hearts: \n자, 오늘도 금요일 주제를 들고 왔는데요, \n바로 '감사하기' 랍니다. 이건 너무나 익숙한 주제이기도 하죠?\n\n감사하기가 좋다는건 누구나 다 아는 사실일텐데요. 정확히 어떻게 좋은지 어떻게 감사하면 좋을지에 대해서 가이딩 코디의 이야기를 들어볼게요.\n\n소중한 다른 사람을 돌봐주듯이 나 자신을 돌봐주며 지내보는거죠.\n\n오늘도 가이딩코디의 이야기를 들어볼게요:rabbit:\n\n:point_down: 오늘의 끼적이기 주제 보기\n https://resonance2heart.notion.site/1-1b3409abaef946b285c976389c1c89ac"},
--     "name":{"S":"22st3-3"},
--     "images":{"SS":["https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-1.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-2.jpg","https://bunnyburrow-community-message-resources.s3.ap-northeast-2.amazonaws.com/program/1%E1%84%8C%E1%85%AE%E1%84%8E%E1%85%A1/fri-3.jpg"]}
-- }

quotMaybe :: Int -> Int -> Maybe Int
quotMaybe x 0 = Nothing
quotMaybe x y = return $ quot x y

-- 쿼트 메이비로 다시 쓰기 . . . 
