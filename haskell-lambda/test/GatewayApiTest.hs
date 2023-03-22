
{-# LANGUAGE OverloadedStrings #-}

module GatewayApiTest 
  ( test 
  ) where  

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
test = withSocketsDo $ do 
  voiceConnData <- WSS.runSecureClient "gateway.discord.gg" 443 "/?v=9&encoding=json" app
  print voiceConnData
  WSS.runSecureClient (endpoint voiceConnData) 443 "/?v=4" (voiceApp voiceConnData)


voiceApp :: VoiceConn -> WS.ClientApp ()
voiceApp voiceConn conn = do 
  putStrLn "voice connection"
  let voiceEst = encode (voiceIdentify voiceConn)
  WS.sendTextData conn voiceEst

  voiceAck <- WS.receiveData conn
  T.putStrLn voiceAck

  readyAck <- WS.receiveData conn 
  T.putStrLn readyAck 


  let address = getData "ip" readyAck 
  let port = getData "port" readyAck 
  let modes = getData "modes" readyAck 

  print address 
  print port
  print modes

  WS.sendTextData conn (encode $ buildUdpConn address port)
  sessDesc <- WS.receiveData conn 
  T.putStrLn sessDesc

  {- readyAck <- WS.receiveData conn
   - T.putStrLn readyAck -}


{-   let hbInterval = getHBInterval . getData "heartbeat_interval" $ voiceAck 
 -
 -   _ <- forkIO . forever $ do
 -     threadDelay (hbInterval*1000)
 -     WS.sendTextData conn (encode vHeartBeat)
 -
 -
 -   let loop = do
 -       msg <- WS.receiveData conn
 -       T.putStrLn msg >> loop
 -
 -   loop -}

  WS.sendClose conn (""::Text)


app :: WS.ClientApp VoiceConn
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

  WS.sendTextData conn (encode voiceConn)

  print "waiting voice state"
  state <- WS.receiveData conn 

  print "waiting voice server"
  server <- WS.receiveData conn 

  print "voice state update text"
  T.putStrLn state

  print "voice server update text"
  T.putStrLn server

  let serverId = getData "guild_id" server
  let userId = getData "user_id" state 
  let sessId = getData "session_id" state 
  let token = getData "token" server 
  let endpoint = getData "endpoint" server 

  print "server id "
  print serverId

  print "user id "
  print userId 

  print "session id"
  print sessId

  print "token"
  print token

  print "endpoint"
  print endpoint

  {- let hbInterval = getHBInterval . getData "heartbeat_interval" $ ack -}

  {- _ <- forkIO . forever $ do 
   -   threadDelay (hbInterval * 1000)
   -   WS.sendTextData conn (encode heartBeat) -}
  
{-   let hbInterval = getHBInterval . getData "heartbeat_interval" $ voiceAck 
 -
 -   _ <- forkIO . forever $ do
 -     threadDelay (hbInterval*1000)
 -     WS.sendTextData conn (encode vHeartBeat)
 -
 -
 -   let loop = do
 -       msg <- WS.receiveData conn
 -       T.putStrLn msg >> loop
 -
 -   loop -}


  WS.sendClose conn (""::Text)
  return $ buildConnForm serverId userId sessId token endpoint


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

