module BotEnvTest where

import qualified Data.Text as T
import System.Environment

get' :: IO T.Text
get' = do
    str <- getEnv "DISCORD_BOT_TOKEN"
    return $ T.pack str

getAppId :: IO String
getAppId =  do 
    str <- getEnv "DISCORD_APP_ID"
    return str



test :: IO ()
test = do
    putStrLn "hello test"