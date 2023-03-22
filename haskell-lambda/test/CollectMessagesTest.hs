
{-# LANGUAGE OverloadedStrings #-}

module CollectMessagesTest where 

import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import qualified Wuss                as WSS
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL

import qualified Data.ByteString as B 

import Network.HTTP.Req
import Data.Aeson 
import Data.Scientific as S
import Data.List.Split as LS

import System.IO

import BotTest 
import JsonTest as Json


test :: IO ()
test = withSocketsDo $ WSS.runSecureClient "gateway.discord.gg" 443 "/?v=9&encoding=json" app
  

app :: WS.ClientApp ()
app conn = do
  putStrLn "Connected!"
  ack <- WS.receiveData conn
  T.putStrLn "Connection ACK"
  T.putStrLn ack

  WS.sendTextData conn (encode identify)
  ready <- WS.receiveData conn
  T.putStrLn "Ready ACK"
  T.putStrLn ready 

  created <- WS.receiveData conn
  T.putStrLn "Guild created ACK"
  T.putStrLn created

  let hbInterval = getHBInterval . getData "heartbeat_interval" $ ack 

  _ <- forkIO . forever $ do 
    threadDelay (hbInterval * 1000)
    WS.sendTextData conn (encode heartBeat)
  
  -- let loop = do
  --     msg <- WS.receiveData conn
  --     msg' <- lookupMsg msg 
  --   --   T.putStrLn msg' >> loop
  --     print msg' >> loop
 
  -- loop


  WS.sendClose conn (""::Text)
  


processMsg :: Text -> IO Text
processMsg msg = return $ "Message from discord: " <> msg


lookupMsg :: Text -> Maybe Text
lookupMsg msg = do
    obj <- getObject msg
    String d <- Json.lookup "d" obj
    return $ "Message from discord whit lookup data: " <> d


getObject :: Text -> Maybe Value 
getObject = decode . TLE.encodeUtf8 . TL.fromStrict 


getHBInterval :: Maybe Value -> Int 
getHBInterval Nothing = 41250
getHBInterval (Just (Number hb)) = toInt hb
getHBInterval _ = 41250


toInt :: S.Scientific -> Int 
toInt x = case S.toBoundedInteger x of 
            Nothing -> 0 
            Just x' -> x'



getData :: Text -> Text -> Maybe Value 
getData key json = do 
  obj <- getObject json 
  d <- Json.lookup "d" obj 
  Json.lookup key d


  
data VoiceConn = VoiceConn { session :: Value
                           , user :: Value
                           , server :: Value 
                           , token :: Value
                           , endpoint :: String 
                           } deriving (Show)

buildConnForm :: Maybe Value -> Maybe Value -> Maybe Value -> Maybe Value -> Maybe Value -> VoiceConn 
buildConnForm (Just server) (Just user) (Just session) (Just token) (Just (String endpoint)) = 
  VoiceConn { session = session 
            , user = user 
            , server = server 
            , token = token 
            , endpoint = head . LS.splitOn ":" . T.unpack $ endpoint 
            }
buildConnForm _ _ _ _ _ = VoiceConn { session = String "" 
                                    , user = String ""
                                    , server = String ""
                                    , token = String ""
                                    , endpoint = ""
                                    } 


data UdpConnPayload = UdpConnPayload { udpAddress :: String 
                                     , udpPort :: Int 
                                     } deriving (Show)

instance ToJSON UdpConnPayload where 
  toJSON (UdpConnPayload address port) = 
    object [ "op" .= Number 1
           , "d" .= object [ "protocol" .= String "udp"
                           , "data" .= object [ "address" .= address 
                                              , "port" .= port 
                                              , "mode" .= String "xsalsa20_poly1305"
                                              ]
                           ]
           ]


buildUdpConn :: Maybe Value -> Maybe Value -> UdpConnPayload 
buildUdpConn (Just (String add)) (Just (Number port)) = UdpConnPayload { udpAddress = T.unpack add 
                                                                       , udpPort = toInt port 
                                                                       }
buildUdpConn _ _ = UdpConnPayload { udpAddress = "", udpPort= -1 } 


voiceIdentify :: VoiceConn -> Value 
voiceIdentify voiceConn = 
  object [ "op" .= Number 0
         , "d" .= object [ "server_id" .= server voiceConn
                         , "user_id" .= user voiceConn
                         , "session_id" .= session voiceConn 
                         , "token" .= token voiceConn
                         ]
         ]


vHeartBeat = object [ "op" .= Number 3
                    , "d" .= Number 1501184119561 
                    ]

heartBeat = object [ "op" .= Number 1 
                   , "d" .= Null 
                   ]

voiceConn = object [ "op" .= Number 4
                   , "d" .= object [ "guild_id" .= String guildID
                                   , "channel_id" .= String musicChannelID
                                   , "self_mute" .= String "false" 
                                   , "self_deaf" .= String "false" 
                                   ]
                   ]


identify = object [ "op" .= Number 2 
                  , "d" .= object [ "token" .= String botToken
                                  , "intents" .= Number (calcIntents fullAvailableIntents )
                                  , "properties" .= object [ "$os" .= String "linux"
                                                           , "$browser" .= String "chrome" 
                                                           , "$device" .= String "pc" 
                                                           ]
                                  ]
                  ]




-- Message from discord: {
--     "t":"MESSAGE_CREATE",
--     "s":15,
--     "op":0,
--     "d":{
--         "type":0,
--         "tts":false,
--         "timestamp":"2022-06-08T15:46:23.117000+00:00",
--         "referenced_message":null,"
--         pinned":false,
--         "nonce":"990292000416727040",
--         "mentions":[],
--         "mention_roles":[],
--         "mention_everyone":false,
--         "member":{
--             "roles":["930655312309018624"],
--             "mute":false,
--             "joined_at":"2022-01-07T05:06:52.068000+00:00",
--             "hoisted_role":"930655312309018624",
--             "flags":0,
--             "deaf":false
--             },
--         "id":"984121220758925364",
--         "flags":0,
--         "embeds":[],
--         "edited_timestamp":null,
--         "content":"토끼굴 마을 떡메모지 프로그램 이번달 신청자 두명에게 보낼 택배 준비완료 낼 배송부칠예정",
--         "components":[],
--         "channel_id":"962375946629943326",
--         "author":{
--             "username":"토끼굴22번길-bunnyburrow22st",
--             "public_flags":0,
--             "id":"925267903836741652",
--             "discriminator":"3428",
--             "avatar_decoration":null,
--             "avatar":"4933ddd0e3a1de2de4c001cd5ed49b22"
--             },
--         "attachments":[{
--             "width":1424,
--             "url":"https://cdn.discordapp.com/attachments/962375946629943326/984121220993781801/20220609_004403.jpg",
--             "size":194847,
--             "proxy_url":"https://media.discordapp.net/attachments/962375946629943326/984121220993781801/20220609_004403.jpg",
--             "id":"984121220993781801",
--             "height":1512,
--             "filename":"20220609_004403.jpg",
--             "content_type":"image/jpeg"
--             }],
--         "guild_id":"928203564180979712"
--         }
--     }
