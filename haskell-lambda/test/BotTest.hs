
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module BotTest where 

import Data.Aeson 
import Network.HTTP.Req

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Scientific as S
import Data.Bits 

botToken :: T.Text 
botToken ="OTQ1MzIyMzc3Mzg0OTYwMDcw.YhOeEg.TzpT7O_OsIpS5gvEqHg1svyI8xo"


appID :: T.Text 
appID ="945322377384960070"

guildID :: T.Text 
guildID ="928203564180979712"

clientID :: T.Text 
clientID ="945322377384960070"

musicChannelID :: T.Text 
musicChannelID = "948223935152005130"


botHeader :: Option scheme
botHeader = header "Authorization" (TE.encodeUtf8 $ "Bot " <> botToken)


data Intents = GUILDS 
             | GUILD_MEMBERS 
             | GUILD_BANS
             | GUILD_EMOJIS_AND_STICKERS
             | GUILD_INTEGRATIONS
             | GUILD_WEBHOOKS
             | GUILD_INVITES
             | GUILD_VOICE_STATES
             | GUILD_PRESENCES
             | GUILD_MESSAGES
             | GUILD_MESSAGE_REACTIONS
             | GUILD_MESSAGE_TYPING
             | DIRECT_MESSAGES
             | DIRECT_MESSAGE_REACTIONS
             | DIRECT_MESSAGE_TYPING 
             | UNDEFINED_0 
             | GUILD_SCHEDULED_EVENTS


getBits :: Intents -> Integer
getBits GUILDS                    = shift 1 0
getBits GUILD_MEMBERS             = shift 1 1
getBits GUILD_BANS                = shift 1 2
getBits GUILD_EMOJIS_AND_STICKERS = shift 1 3
getBits GUILD_INTEGRATIONS        = shift 1 4
getBits GUILD_WEBHOOKS            = shift 1 5
getBits GUILD_INVITES             = shift 1 6
getBits GUILD_VOICE_STATES        = shift 1 7
getBits GUILD_PRESENCES           = shift 1 8
getBits GUILD_MESSAGES            = shift 1 9
getBits GUILD_MESSAGE_REACTIONS   = shift 1 10
getBits GUILD_MESSAGE_TYPING      = shift 1 11
getBits DIRECT_MESSAGES           = shift 1 12
getBits DIRECT_MESSAGE_REACTIONS  = shift 1 13
getBits DIRECT_MESSAGE_TYPING     = shift 1 14
getBits GUILD_SCHEDULED_EVENTS    = shift 1 16

                           
calcIntents :: [Intents] -> S.Scientific 
calcIntents is = fromInteger . sum . map getBits $ is


fullAvailableIntents = [  GUILDS                   
                       ,  GUILD_BANS
                       ,  GUILD_EMOJIS_AND_STICKERS
                       ,  GUILD_INTEGRATIONS
                       ,  GUILD_WEBHOOKS
                       ,  GUILD_INVITES
                       ,  GUILD_VOICE_STATES
                       ,  GUILD_MESSAGES
                       ,  GUILD_MESSAGE_REACTIONS
                       ,  GUILD_MESSAGE_TYPING
                       ,  DIRECT_MESSAGES
                       ,  DIRECT_MESSAGE_REACTIONS
                       ,  DIRECT_MESSAGE_TYPING 
                       ,  GUILD_SCHEDULED_EVENTS
                       ]

allIntents = [  GUILDS                   
             ,  GUILD_MEMBERS
             ,  GUILD_BANS
             ,  GUILD_EMOJIS_AND_STICKERS
             ,  GUILD_INTEGRATIONS
             ,  GUILD_WEBHOOKS
             ,  GUILD_INVITES
             ,  GUILD_VOICE_STATES
             ,  GUILD_PRESENCES
             ,  GUILD_MESSAGES
             ,  GUILD_MESSAGE_REACTIONS
             ,  GUILD_MESSAGE_TYPING
             ,  DIRECT_MESSAGES
             ,  DIRECT_MESSAGE_REACTIONS
             ,  DIRECT_MESSAGE_TYPING 
             ,  GUILD_SCHEDULED_EVENTS
             ]
