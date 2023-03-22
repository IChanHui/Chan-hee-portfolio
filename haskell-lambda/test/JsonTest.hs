
{-# LANGUAGE OverloadedStrings #-}

module JsonTest 
  ( JsonTest.lookup
  ) where 

import Data.Aeson
import Data.Aeson.Types
import Data.Text 
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TLIO


testData :: Value 
testData = object [ "test1" .= String "test2"
                  , "test2" .= String "test2"
                  ]


value :: Value -> IO ()
value (Object obj) = case parse (.: "test1") obj of
                       Error e -> putStrLn e
                       Success value -> TLIO.putStrLn . TLE.decodeUtf8 . encode $ (value :: Value)

lookup' :: Text -> Object -> Maybe Value
lookup' key obj = case parse (.: key) obj of 
                    Error e -> Nothing 
                    Success value -> Just value 

test :: Value -> IO () 
test (Object obj) = case JsonTest.lookup' "test1" obj of 
         Nothing -> putStrLn "Fail to parse json"
         Just value -> consoleLog value

test _ = putStrLn "Fail to get Object"
  
lookup :: Text -> Value -> Maybe Value
lookup key (Object obj) = case parse (.: key) obj of 
                   Error e -> Nothing 
                   Success value -> Just value 
lookup _ _ = Nothing 


consoleLog = TLIO.putStrLn . TLE.decodeUtf8 . encode 



-- -- json
-- "{\"app_permissions\":\"4398046511103\",
-- \"application_id\":\"945322377384960070\",
-- \"channel_id\":\"945510596093292564\",
-- \"data\":{
--   \"guild_id\":\"928203564180979712\",
--   \"id\":\"1013101585724878899\",
--   \"name\":\"끼적이기\",
--   \"options\":[{
--     \"name\":\"멤버\",
--     \"type\":6,
--     \"value\":\"602853327784640532\"
--   }],
--   \"resolved\":{
--     \"members\":{
--       \"602853327784640532\":{
--         \"avatar\":null,
--         \"communication_disabled_until\":null,
--         \"flags\":0,
--         \"is_pending\":false,
--         \"joined_at\":\"2022-01-05T08:35:21.171000+00:00\",
--         \"nick\":\"lee chanhee\",
--         \"pending\":false,
--         \"permissions\":\"4398046511103\",
--         \"premium_since\":null,
--         \"roles\":[\"930655312309018624\"]
--       }
--     },
--     \"users\":{
--       \"602853327784640532\":{
--         \"avatar\":\"22de5f40da6b4ea26c113199dd03fbb9\",
--         \"avatar_decoration\":null,
--         \"discriminator\":\"7241\",
--         \"id\":\"602853327784640532\",
--         \"public_flags\":0,
--         \"username\":\"Baaaam\"
--       }
--     }
--   },
--   \"type\":1
-- },
-- \"guild_id\":\"928203564180979712\",
-- \"guild_locale\":\"en-US\",
-- \"id\":\"1013431969767510038\",
-- \"locale\":\"en-US\",
-- \"member\":{
--   \"avatar\":null,
--   \"communication_disabled_until\":null,
--   \"deaf\":false,
--   \"flags\":0,
--   \"is_pending\":false,
--   \"joined_at\":\"2022-01-05T08:29:34.913000+00:00\",
--   \"mute\":false,
--   \"nick\":null,
--   \"pending\":false,
--   \"permissions\":\"4398046511103\",
--   \"premium_since\":null,
--   \"roles\":[\"930655312309018624\"],
--   \"user\":{
--     \"avatar\":\"3ec029423e65ce9d7775059264572166\",
--     \"avatar_decoration\":null,
--     \"discriminator\":\"5008\",
--     \"id\":\"687326243703881793\",
--     \"public_flags\":0,
--     \"username\":\"leechanwoo\"
--   }
-- },
-- \"token\":\"aW50ZXJhY3Rpb246MTAxMzQzMTk2OTc2NzUxMDAzODpRTTE5NHozWnA5c2RUQ0phdzNGTHk5cGp4QUxjVDZmWHdZU1J4NFk1ZHhENE9GbldxbVh4Nlg5dExhdkZzYnU2eEZ6eThrNnQ5RUtjcWl6TWlpQWJyeXFQM3B3NWd5NjFySUdNbHg4MDBXRmI3TnlhaVpBSXZsdjl6OXZKRWUzcg\",
-- \"type\":2,
-- \"version\":1
-- }"

-- -- lee chanhee -> @lee chanwoo
-- bb :: String
-- bb = 
-- "{\"app_permissions\":\"4398046511103\",
-- \"application_id\":\"945322377384960070\",
-- \"channel_id\":\"945510596093292564\",
-- \"data\":{
--   \"guild_id\":\"928203564180979712\",
--   \"id\":\"1013101585724878899\",
--   \"name\":\"끼적이기\",
--   \"options\":[{
--     \"name\":\"멤버\",
--     \"type\":6,
--     \"value\":\"602853327784640532\"
--   }],
--   \"resolved\":{
--     \"members\":{
--       \"602853327784640532\":{
--         \"avatar\":null,
--         \"communication_disabled_until\":null,
--         \"flags\":0,
--         \"is_pending\":false,
--         \"joined_at\":\"2022-01-05T08:35:21.171000+00:00\",
--         \"nick\":\"lee chanhee\",
--         \"pending\":false,
--         \"permissions\":\"1071698660929\",
--         \"premium_since\":null,
--         \"roles\":[]
--       }
--     },
--     \"users\":{
--       \"602853327784640532\":{
--         \"avatar\":\"22de5f40da6b4ea26c113199dd03fbb9\",
--         \"avatar_decoration\":null,
--         \"discriminator\":\"7241\",
--         \"id\":\"602853327784640532\",
--         \"public_flags\":0,
--         \"username\":\"Baaaam\"
--       }
--     }
--   },
--   \"type\":1
-- },
-- \"guild_id\":\"928203564180979712\",
-- \"guild_locale\":\"en-US\",
-- \"id\":\"1013347243870392411\",
-- \"locale\":\"ko\",
-- \"member\":{
--   \"avatar\":null,
--   \"communication_disabled_until\":null,
--   \"deaf\":false,
--   \"flags\":0,
--   \"is_pending\":false,
--   \"joined_at\":\"2022-01-05T08:35:21.171000+00:00\",
--   \"mute\":false,
--   \"nick\":\"lee chanhee\",
--   \"pending\":false,
--   \"permissions\":\"1071698660929\",
--   \"premium_since\":null,
--   \"roles\":[],
--   \"user\":{
--     \"avatar\":\"22de5f40da6b4ea26c113199dd03fbb9\",
--     \"avatar_decoration\":null,
--     \"discriminator\":\"7241\",
--     \"id\":\"602853327784640532\",
--     \"public_flags\":0,
--     \"username\":\"Baaaam\"
--   }
-- },
-- \"token\":\"aW50ZXJhY3Rpb246MTAxMzM0NzI0Mzg3MDM5MjQxMTpXVFBhM1FkbEZHUWFjak8wNXViZWFxYnZyRlhRcUp2dzFIaFd5dEhBYVNJVTA3UVB0YWtEMENTcGhteHdGcWEzRlg1cHlVdktOUDhTdXRtbXFCNDhXQkRKQUNzdW5EWGc5QlJrSE4wdDlDbGU4VE8wdUNkNnV4d3NmeDl0alN4Sg\",
-- \"type\":2,
-- \"version\":1
-- }"

-- -- decode