{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module CommandControlTest
    ( printAllCommands
    , addNewCommand
    , deleteCommandByName
    , deleteAllCommands
    ) where


import Control.Monad.IO.Class
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Req
import qualified Data.Vector as V 

import qualified Data.Text as T 
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B
import System.Environment


appID :: IO T.Text 
appID = do
    str <- getEnv "DISCORD_APP_ID"
    return $ T.pack str

guildID :: IO T.Text 
guildID = do
    str <- getEnv "DISCORD_GUILD_ID"
    return $ T.pack str

botToken :: IO T.Text 
botToken = do
    str <- getEnv "DISCORD_BOT_TOKEN"
    return $ T.pack str

clientID :: IO T.Text 
clientID = do
    str <- getEnv "DISCORD_CLIENT_ID"
    return $ T.pack str

botHeader :: IO (Option scheme)
botHeader = do  
    botToken' <- botToken
    return $ header "Authorization" (TE.encodeUtf8 $ "Bot " <> botToken')

-- appId :: String
-- appId = "945322377384960070"

-- guildId :: String 
-- guildId = "928203564180979712"

-- botToken :: String 
-- botToken = "OTQ1MzIyMzc3Mzg0OTYwMDcw.YhOeEg.TzpT7O_OsIpS5gvEqHg1svyI8xo"

-- clientId :: String 
-- clientId = "945322377384960070"


-- botHeader :: Option scheme
-- botHeader = header "Authorization" (B.pack $ "Bot " ++ botToken)

type CommandName = String 
type Description = String 
type Key = String

url :: IO (Url 'Https)
url = do 
  appID' <- appID
  guildID' <- guildID
  return $ https "discord.com" 
            /: "api" 
            /: "v10" 
            /: "applications" 
            /: appID'
            /: "guilds" 
            /: guildID'
            /: "commands"

-- /applications/{application.id}/guilds/{guild.id}/commands

fetchCommands :: IO Value 
fetchCommands = do 
  url' <- url
  botHeader' <- botHeader
  runReq defaultHttpConfig $ do
  r <- req GET url' NoReqBody jsonResponse botHeader'
  liftIO $ return (responseBody r :: Value)


addNewCommand :: CommandName -> Description -> IO () 
addNewCommand name desc = do
  url' <- url
  botHeader' <- botHeader
  runReq defaultHttpConfig $ do
  let payload =
        object
          [ "name" .= name 
          , "description" .= desc 
          , "type".= (1 :: Int)
          , "options" .= ([] :: [String])
          ]
  r <- req POST url' (ReqBodyJson payload) jsonResponse botHeader'
  liftIO $ print (responseBody r :: Value) 

addNewSubCommand :: CommandName -> Description -> CommandName -> Description -> IO () 
addNewSubCommand name desc subName subDesc = do 
  url' <- url
  botHeader' <- botHeader
  runReq defaultHttpConfig $ do
  let payload =
        object
          [ "name" .= name 
          , "description" .= desc 
          , "type".= (1 :: Int)
          , "options" .= ([
            object [
              "name" .= subName 
            , "description" .= subDesc 
            , "type".= (1 :: Int)
            ]
          ])
          ]
  r <- req POST url' (ReqBodyJson payload) jsonResponse botHeader'
  liftIO $ print (responseBody r :: Value) 


-- payload msg = object [ "name" .= name, "description" .= desc, "type".= (1 :: Int), "options" .= ([] :: [String])]
addgameSubCommand :: IO () 
addgameSubCommand = do 
  url' <- url
  botHeader' <- botHeader
  runReq defaultHttpConfig $ do
  let payload =
        object
          [ "name" .= ("game" :: CommandName) 
          , "description" .= ("playing game" :: Description) 
          , "type".= (1 :: Int)
          , "options" .= ([
            object [
              "name" .= ("tetris" :: CommandName) 
            , "description" .= ("tetris game" :: Description)  
            , "type".= (1 :: Int)
            ],
            object [
              "name" .= ("cube" :: CommandName) 
            , "description" .= ("cube game" :: Description)  
            , "type".= (1 :: Int)
            ]
            ])
          ]
  r <- req POST url' (ReqBodyJson payload) jsonResponse botHeader'
  liftIO $ print (responseBody r :: Value)

addNewSubCommand' :: IO () 
addNewSubCommand' = do 
  url' <- url
  botHeader' <- botHeader
  runReq defaultHttpConfig $ do
  let payload =
        object
          -- [ "name" .= ("카페주문" :: CommandName) 
          [ "name" .= ("식당주문" :: CommandName) 
          , "description" .= ("주문 도와드리겠습니다~" :: Description) 
          , "type".= (1 :: Int)
          , "options" .= ([

            -- object [
            --   "name" .= ("구름아이스크림" :: CommandName) 
            -- , "description" .= ("구름처럼 부드러우 아이스크림~ 아기토끼와 함께~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("단백질음료" :: CommandName) 
            -- , "description" .= ("무려 담백질 함유 12g!" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("마그네슘물" :: CommandName) 
            -- , "description" .= ("에너지 음료수같은 포장인데 그냥 물!" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("바나나초코파이" :: CommandName) 
            -- , "description" .= ("바나나 덕후는 무조건 도전~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("바닐라크림도넛" :: CommandName) 
            -- , "description" .= ("바닐라 커스터드 크림이 듬뿍든 도넛~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("베이글" :: CommandName) 
            -- , "description" .= ("커피와 베이글 조합은 언제나 맛있는 것 같아요~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("신호등음료수" :: CommandName) 
            -- , "description" .= ("알록달록한데 시원하고 맛있어요~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("에이스뉴욕치즈케익맛" :: CommandName) 
            -- , "description" .= ("고소한 에이스에 은은 꾸떡한 치즈케이크 맛이 커피를 부르네~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("오레오스콘" :: CommandName) 
            -- , "description" .= ("달달한 오레오 스콘과 따뜻한 커피~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("왕수박바" :: CommandName) 
            -- , "description" .= ("수박바 아이스크림~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("조각케익" :: CommandName) 
            -- , "description" .= ("차나 커피와 함께 먹기 좋은 조각케익~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("체리도넛" :: CommandName) 
            -- , "description" .= ("크림을 듬뿍 품은 도넛에 체리~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("초코볼" :: CommandName) 
            -- , "description" .= ("초코볼 케이스에 모자가 달렸네?" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("초코와플" :: CommandName) 
            -- , "description" .= ("바삭한 와플에 달달한 초코시럽~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("카페라떼" :: CommandName) 
            -- , "description" .= ("부드러운 우유와 에소프레소의 조합~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("토끼쿠키" :: CommandName) 
            -- , "description" .= ("히힣~ 귀여운 토끼쿠키이이~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("푸린포켓몬빵" :: CommandName) 
            -- , "description" .= ("푸린의 피치피치슈~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("하리보젤리" :: CommandName) 
            -- , "description" .= ("쫄깃한 식감이 특징인 하리보젤리~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]
            -- , object [
            --   "name" .= ("핫아메리카노" :: CommandName) 
            -- , "description" .= ("따듯하게 몸을 녹여주고 향으로 안정을 주는 아메리카노~" :: Description)  
            -- , "type".= (1 :: Int)
            -- ]

            object [
              "name" .= ("감바스" :: CommandName) 
            , "description" .= ("왕 큰 새우가 들어간 촉촉한 감바스~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("고기듬뿍마라" :: CommandName) 
            , "description" .= ("고기가 한 움큼들어간 마라~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("김치치즈볶음밥" :: CommandName) 
            , "description" .= ("김볶에 치즈~ 맛 없을 수 없는 조합~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("닭갈비" :: CommandName) 
            , "description" .= ("한번도 안 먹어본 사람은 있어도 한번만 먹은 사람 없는 닭갈비~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("돈까스한상" :: CommandName) 
            , "description" .= ("바삭한 꼬소한 돈까스 정식~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("떡볶이김밥세트" :: CommandName) 
            , "description" .= ("떡볶이 국물에 국물을 적셔 먹으면 크~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("라면김밥세트" :: CommandName) 
            , "description" .= ("남은 라면 국물에 남은 김밥 말아먹어도 굴맛~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("라자냐" :: CommandName) 
            , "description" .= ("부드러운 소스와 치즈가 굴맛인 라자냐~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("롤만두" :: CommandName) 
            , "description" .= ("길쭉한 모양의 맛있는 롤만두~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("마라매운맛" :: CommandName) 
            , "description" .= ("매콤하고 중독적인 마라수혈시간~" :: Description)  
            , "type".= (1 :: Int)
            ]

            , object [
              "name" .= ("마라샹궈" :: CommandName) 
            , "description" .= ("마라향을 극대화로 즐기는 방법~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("만두국" :: CommandName) 
            , "description" .= ("만둣국에 순두부 넣어 먹어도 맛있더라고요~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("면듬뿍마라" :: CommandName) 
            , "description" .= ("마라와 면의 조합은 싫을 수가 없다~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("반반치킨" :: CommandName) 
            , "description" .= ("진리의 후라이드 반 양념 반~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("부대찌개" :: CommandName) 
            , "description" .= ("라면사리 추가한 부대찌개~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("비빔미역국수" :: CommandName) 
            , "description" .= ("미역국수에 비빔소스를 곁들여서 더 맛난 비빔국수~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("뼈해장국" :: CommandName) 
            , "description" .= ("얼큰한 국물와 부드러운 고기가 환상적~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("샐러드" :: CommandName) 
            , "description" .= ("다채로운 채소를 맛있게 먹을 수 있는 샐러드~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("소시지미역국라면" :: CommandName) 
            , "description" .= ("오해 말아요~ 소고기가 없어서 소시지 넣은 거에요~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("순대국밥" :: CommandName) 
            , "description" .= ("뜨끈한 뽀얀 국물에 순대까지~" :: Description)  
            , "type".= (1 :: Int)
            ]

            , object [
              "name" .= ("스시세트" :: CommandName) 
            , "description" .= ("맛있는 스시세트에 사이드로 콘치즈~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("제육볶음" :: CommandName) 
            , "description" .= ("입맛 없을 때 갓 지은 밥에 제육볶음~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("콩국수" :: CommandName) 
            , "description" .= ("걸쭉하고 고소한데 시원한 콩국수~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("크림리조또" :: CommandName) 
            , "description" .= ("부드러운 크림으로 리조또~ 토핑으로 치즈까지~" :: Description)  
            , "type".= (1 :: Int)
            ]
            , object [
              "name" .= ("파인애플피자" :: CommandName) 
            , "description" .= ("따뜻한 파인애플과 피자의 조합~" :: Description)  
            , "type".= (1 :: Int)
            ]
          ])
          ]
  r <- req POST url' (ReqBodyJson payload) jsonResponse botHeader'
  liftIO $ print (responseBody r :: Value)


addArgumentCommand :: IO ()
addArgumentCommand = do 
  url' <- url
  botHeader' <- botHeader
  runReq defaultHttpConfig $ do
  let payload =
        object
          [ "name" .= ("출석삭제" :: CommandName) 
          , "description" .= ("운영자만 사용이 가능합니다." :: Description) 
          , "type".= (1 :: Int)
          , "options" .= ([
            object [
              "name" .= ("멤버" :: CommandName) 
            , "description" .= ("최근 출석일을 삭제할 멤버를 알려주세요." :: Description)  
            , "type".= (6 :: Int)
            , "required" .= Bool True
            ]
            

        -- object
        --   [ "name" .= ("오늘의주제" :: CommandName) 
        --   , "description" .= ("운영자만 사용이 가능합니다. 오늘의 끼적이기 주제를 올립니다." :: Description) 
        --   , "type".= (1 :: Int)
        --   , "options" .= ([
        --       -- object [
        --       --     "name" .= ("age" :: CommandName)
        --       --   , "type" .= (4 :: Int)
        --       --   , "description" .= ("진행중인 끼적이기 쓰레드ID를 알려주세요." :: Description) 
        --       -- ]


        --     object [
        --       "name" .= ("쓰레드id" :: CommandName) 
        --     , "description" .= ("진행중인 끼적이기 쓰레드ID를 알려주세요." :: Description)  
        --     , "type".= (3 :: Int)
        --     , "required" .= Bool True
        --     ]
        
            ])
          ]
  r <- req POST url' (ReqBodyJson payload) jsonResponse botHeader'
  liftIO $ print (responseBody r :: Value)

extractId :: Value -> V.Vector String
extractId (Array arr) = do 
  Object obj <- arr -- TODO: unsafe
  Just id <- return $ parseMaybe (obj .:) "id"  -- TODO: unsafe
  return id 

extractNameId :: Value -> [(String, String)]
extractNameId (Array arr) {- TODO: non-exhausted -} = do
  Object obj <- V.toList arr -- TODO: unsafe 
  let [Just name, Just id] = parseMaybe (obj .:) . T.pack <$> ["name", "id"]  -- TODO: unsafe 
  return (name, id)

deleteCommand :: String -> IO () 
deleteCommand id = do 
  url' <- url
  botHeader' <- botHeader
  runReq defaultHttpConfig $ do
  r <- req DELETE (url' /: T.pack id) NoReqBody ignoreResponse botHeader'
  liftIO $ print (responseStatusCode r)

deleteCommandByName :: String -> IO () 
deleteCommandByName name = 
  fetchCommands >>= 
    deleteCommand 
    . snd 
    . head  -- TODO: unsafe
    . filter ((==name) . fst) 
    . extractNameId

deleteAllCommands :: IO () 
deleteAllCommands = do 
  commands <- fetchCommands 
  mapM_ deleteCommand (extractId commands)


deleteFstCommand :: IO ()
deleteFstCommand = do
  id <- fetchCommands
  deleteCommand . V.head . extractId $ id -- TODO: unsafe

genCommands :: IO () 
genCommands = forM_  [1..5] $ \i -> 
  addNewCommand ("test"++show i) ("test" ++ show i ++ " command")

printAllCommands :: IO ()
printAllCommands = fetchCommands >>= print . extractNameId


