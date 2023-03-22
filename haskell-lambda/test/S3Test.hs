
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE OverloadedStrings #-}



module S3Test where

import           Control.Exception.Lens (handling)
import           Control.Lens ((<&>), (^.), (.~), (&), (?~), set)
import           Control.Monad.Trans.AWS
                    ( AWST'
                    , runAWST
                    , Credentials(..)
                    , Env
                    , LogLevel(..)
                    , Region(..)
                    , Service
                    , envLogger
                    , newEnv
                    , newLogger
                    , reconfigure
                    , runResourceT
                    , setEndpoint
                    , within
                    )
import           Control.Monad.Trans.Resource (MonadUnliftIO, ResourceT)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.Text as T (null, pack, unpack, Text)
import qualified Data.Text.IO as Text (putStrLn)
import           System.IO hiding (hGetContents)
import           Control.Monad (forM_, void, when)
import           Data.Conduit.Binary (sinkLbs)
import           Data.Monoid ((<>))


import           Network.AWS.Data (toText)
import           Network.AWS.Data.Body (toBody)
import           Network.AWS.S3
                    (
                    getObject
                    ,goBucket
                    , _BucketAlreadyOwnedByYou
                    , BucketName(..)
                    , LocationConstraint(..)
                    , ObjectKey(..)
                    , bucketExists
                    , cbCreateBucketConfiguration
                    , cbcLocationConstraint
                    , createBucketConfiguration
                    , bName
                    , createBucket
                    , gorsBody
                    , headBucket
                    , lbrsBuckets
                    , listBuckets
                    , listObjectsV2
                    , lovrsContents 
                    , oKey
                    , putObject
                    , s3
                    )
import           Network.AWS
                    ( Region(..)
                    , Service
                    , await
                    , await
                    , send
                    , sinkBody
                    )

import           System.IO (stdout)


type HostName = ByteString

type Port = Int

data AWSInfo = AWSInfo
    { env :: Env
    , region :: Region
    , service :: Service
    }

data S3Info = S3Info
    { aws :: AWSInfo
    , bucketName :: BucketName
    }

data LoggingState = LoggingEnabled | LoggingDisabled

data ServiceEndpoint = AWS Region | Local HostName Port



getS3Info :: LoggingState -> ServiceEndpoint -> IO S3Info
getS3Info loggingState serviceEndpoint = do
    aws <- getAWSInfo loggingState serviceEndpoint s3
    return $ S3Info aws "/bunnyburrow-community-message-resources"

doCreateBucketIfNotExists :: S3Info -> IO ()
doCreateBucketIfNotExists S3Info{..} = withAWS' aws $ do
    let cbc = createBucketConfiguration
                & cbcLocationConstraint .~ Just (LocationConstraint (region aws))
    newlyCreated <- handling _BucketAlreadyOwnedByYou (const (pure False)) $ do
        void $ send $ createBucket bucketName
                        & cbCreateBucketConfiguration .~ Just cbc
        return True
    when newlyCreated (void $ await bucketExists (headBucket bucketName))

doListBuckets :: S3Info -> IO [BucketName]
doListBuckets S3Info{..} = withAWS' aws $ do
    result <- send $ listBuckets
    return $ [ x ^. bName | x <- result ^. lbrsBuckets ]

doPutObject :: S3Info -> IO ()
doPutObject S3Info{..} = do 
    handle <- openFile "./resource/sticker.png" ReadMode
    rqBody <- BL.hGetContents handle
    withAWS' aws $ do
    void $ send $ putObject bucketName (ok "sticker.png") (toBody rqBody)

doListObjects :: S3Info -> IO [ObjectKey]
doListObjects S3Info{..} = withAWS' aws $ do
    result <- send $ listObjectsV2 bucketName
    return $ [ x ^. oKey | x <- result ^. lovrsContents ]


ok :: T.Text -> ObjectKey
ok = ObjectKey


doGetObject :: S3Info -> IO BL.ByteString
doGetObject S3Info{..} = withAWS' aws $ do
    result <- send $ getObject bucketName "for-lambda/31b3f7c2e723216d6385e962d1941ad5.jpeg"
    (result ^. gorsBody) `sinkBody` sinkLbs

test :: IO ()
test = do
    -- Use the real thing
    s3Info <- getS3Info LoggingDisabled (AWS Seoul)

    -- localstack by default exposes its S3 service on port 4572
    --s3Info <- getS3Info LoggingDisabled (Local "localhost" 4572)

    -- putStrLn "CreateBucket"
    -- doCreateBucketIfNotExists s3Info

    -- putStrLn "ListBuckets"
    -- bucketNames <- doListBuckets s3Info
    -- forM_ bucketNames $ \n ->
    --     Text.putStrLn $ "  " <> toText n

    putStrLn "PutObject"
    doPutObject s3Info

    -- putStrLn "ListObjects"
    -- objectKeys <- doListObjects s3Info
    -- forM_ objectKeys $ \k ->
    --     Text.putStrLn $ "  " <> toText k

    -- putStrLn "GetObject"
    -- content <- doGetObject s3Info
    -- BL.putStrLn $ "  " <> content

getAWSInfo :: LoggingState -> ServiceEndpoint -> Service -> IO AWSInfo
getAWSInfo loggingState serviceEndpoint service = do
    e <- getEnv loggingState
    let (r, s) = regionService serviceEndpoint service
    return $ AWSInfo e r s
    where
        -- Standard discovery mechanism for credentials, log to standard output
        getEnv LoggingEnabled = do
            logger <- newLogger Debug stdout
            newEnv Discover <&> set envLogger logger
        -- Standard discovery mechanism for credentials, no logging
        getEnv LoggingDisabled = newEnv Discover

        -- Run against a DynamoDB instance running on AWS in specified region
        regionService (AWS region) s = (region, s)
        -- Run against a local DynamoDB instance on a given host and port
        regionService (Local hostName port) s = (NorthVirginia, setEndpoint False hostName port s)


withAWS :: MonadUnliftIO m => AWST' Env (ResourceT m) a -> AWSInfo -> m a
withAWS action AWSInfo{..} =
    runResourceT . runAWST env . within region $ do
        reconfigure service action

withAWS' :: MonadUnliftIO m => AWSInfo -> AWST' Env (ResourceT m) a -> m a
withAWS' = flip withAWS




a :: Int -> IO ()
a int = print int