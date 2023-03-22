
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module ActionTest where 

import Data.Aeson 
import Network.HTTP.Req 
import Control.Monad.Trans (liftIO)

import qualified Data.Text as T 

import BotTest 

discord :: Url 'Https
discord = https "discord.com" /: "api" /: "v8"


createMessageUrl :: T.Text -> Url 'Https
createMessageUrl ch = discord /: "channels" /: ch /: "messages"

getGatewayUrl :: Url 'Https
getGatewayUrl = discord /: "gateway"


createGuildScheduledEvent :: Url 'Https
createGuildScheduledEvent = discord /: "guilds" /: guildID /: "scheduled-events"


messageObject :: T.Text -> Value 
messageObject msg = object [ "content" .= String msg
                           , "tts" .= Bool False 
                           ]
class Api a where 
  request :: a -> IO Value 


data Action = CreateMessage T.Text T.Text 
            | GetGateway 
  deriving (Show)


instance Api Action where 
  request (CreateMessage id msg) = runReq defaultHttpConfig $ do
    r <- req POST 
             (createMessageUrl id) 
             (ReqBodyJson $ messageObject msg) 
             jsonResponse 
             botHeader

    liftIO $ return (responseBody r :: Value) 
    
  request GetGateway = runReq defaultHttpConfig $ do 
    r <- req GET getGatewayUrl NoReqBody jsonResponse mempty 
    liftIO $ return (responseBody r :: Value)

