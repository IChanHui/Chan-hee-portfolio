
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RollsTest
    ( 
    ) where

import Data.Aeson

import Text.RawString.QQ
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T 
import Data.Time.Clock 
import Data.Scientific 

import System.Random 
import System.Random.Shuffle (shuffle')

import Timestamp
import Message
import MessageBuilder
import Response
import Json


-- testDice1To6 :: IO Int
-- testDice1To6 = randomRIO (1,6)

morningMsg' = ["hello", "hihi", "morning", "welcome", "lol", "aaaaa"]

helloMsg' :: [String] -> IO String
helloMsg' msg = do 
  gen <- newStdGen
  return $ head $ shuffle' msg (length msg) gen



test = testMessageProc (Just simulatingMsgInput)

simulatingMsgInput = object [ "type" .= Number 4]


testMessageProc :: Maybe Value -> IO Response
testMessageProc Nothing = return verifyingErr
testMessageProc (Just body) = 
  case msgType of 
    Nothing -> return $ missingError "type" 
    Just 1 -> return $ ping  
    Just _ -> commandProc body

  where 
    msgType = do 
      Number msgType <- Json.lookup "type" body 
      toBoundedInteger msgType :: Maybe Int

    commandProc = commandReply' . getCommand 


commandReply' :: Maybe T.Text -> IO Response 
commandReply' (Just "go") = return testGoMsg 
commandReply' (Just "guiding") = return testGuidingMsg 
commandReply' (Just "codey") = testRolling 
commandReply' _ = return testDefaultMsg 

testRollsMsg' :: Int -> IO Response 
testRollsMsg' diceVal = return . buildSuccess $ buildReply (String $ T.pack ("dice result : " ++ (show diceVal)))



testRolling :: IO Response
testRolling = do
    diceResult <- dice1To6
    testMst <- testRollsMsg' diceResult
    return testMst


-- funIO :: IO String
-- funIO = print "hello"

-- dice1To6 = randomRIO ("1","6") :: IO String
testDice1To6 :: IO Int
testDice1To6 = randomRIO (1,6)
-- dice1To6 = randomRIO (1,6) :: IO Int

-- nowTime = getCurrentTime >>= return . rolls . timestampMicroSecondsInt64 . utcTimeTimestamp

-- rolls :: (Num a, Random a) => Int -> a
-- rolls n = fst $ randomR (1, 6) (mkStdGen n)



--  rollDice :: IO Int
--  rollDice = getStdRandom (randomR (1,6))

-- print $ take 10 (randomList 42 :: [Float])

-- randomList :: (Random a) => Int -> Int
-- randomList seed = randoms (mkStdGen seed)

-- gen = mkStdGen 1

-- (seed, gen') = next gen
-- randomR (1, 6) gen'

-- (seed, gen'') = next gen'
-- randomR (1, 6) gen''

-- (seed, gen''') = next gen''
-- randomR (1, 6) gen'''

-- rolls :: RandomGen g => Int -> g -> Int
-- rolls n = 



-- rolls :: RandomGen g => Int -> g -> [Word]
-- rolls n = take n . unfoldr (Just . uniformR (1, 6))

-- pureGen = mkStdGen 139












