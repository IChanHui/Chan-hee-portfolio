 {-# LANGUAGE OverloadedStrings #-}


module CronTest where

import           Data.Text (Text)
import qualified Data.Text as Text (null, pack, unpack)
import System.Cron hiding (dayOfWeek)
import System.Cron.Types hiding (dayOfWeek)
import System.Cron.Schedule
import System.Cron.Parser
import System.Cron.Internal.Schedule
import Data.Either
import Data.Time.Clock

import Data.DateTime (fromGregorian, toSeconds, fromSeconds)
import Data.Time hiding (fromGregorian)
import Data.Time.Calendar hiding (fromGregorian)
import Data.Time.Calendar.WeekDate as CalWee 
import Data.Scientific
import Data.Aeson

-- import DynamoDbTest
import MaybeFunctionTest
import DayOfWeekMessageTest


-- db = proIngfo -> 보낼시간, 보낼분, 릴리즈 or 테스트
-- 릴리즈 -> 월 수 금 오전10 -> 시간 기준으로 날짜가 바뀌는 구간
-- -- 테스트 -> 월 수 금 오전10 -> 시간 기준으로 날짜가 바뀌는 구간
-- 테스트 -> 하루 9개 -> 날짜는 기준으로 시간이 바뀌는 구간

-- db -> programInfo에 룰즈를 같이 저장해야겠네



dbText :: Text
dbText = Text.pack "0 1 * * 1,3,5"
-- dbText = Text.pack "* 1 ? * 2,4,6"
-- dbText = Text.pack "* 1 ? * mon,wed,fri"

printIO :: IO ()
printIO = putStrLn "hello"

test' :: IO (CronSchedule)
test' = do

    let (Right a) = parseCronSchedule dbText
    return a
    -- scheduleMatches a time

-- (Just millisecond) = getNumber (Number 1663894818495) -- fri
(Just millisecond) = getNumber (Number 1663722049325) -- wed
-- (Just millisecond) = getNumber (Number 1663549218490) -- mon
(Just milliS) = quotMaybe (coefficient millisecond) 1000
today = fromSeconds milliS

(Right a) = parseCronSchedule dbText
test = scheduleMatches a today

localday = utcToLocalTime (hoursToTimeZone 9) today
todayDay = localDay localday
checkWeek = dayOfWeek todayDay

(Just test2) = nextMatch a today 

utcRange :: Int -> UTCTime -> [UTCTime]
utcRange 18 today = [today']
    where
        (Right a) = parseCronSchedule dbText
        (Just today') = nextMatch a today
utcRange int today = today' : (utcRange (succ int) today')
    where
        (Right a) = parseCronSchedule dbText
        (Just today') = nextMatch a today


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








