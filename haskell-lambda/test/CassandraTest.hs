{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}


module CassandraTest where  


import           Data.Text           (Text)
import qualified Data.Text           as T
import Data.Functor.Identity
import Database.CQL.IO as Client
-- import           Cassandra
-- import           Database.Cassandra
-- import           Database.Cassandra.CQL
-- import           Database.Cassandra.Pack
-- import           Database.Cassandra.Types
-- import Database.CQL.Protocol.Class
-- import Database.CQL.Protocol.Codec (putValue, getValue)
-- import Database.CQL.Protocol.Types
import qualified System.Log.Logger as Logger
-- import           Database.Cassandra.Thrift.Cassandra


test :: IO ()
test = putStrLn "hello"

-- defSettings

-- newtype UserId = UserId UUID deriving (Eq, Show, CasType)

-- instance CasType User where
--     getCas = decode . unBlob <$> getCas
--     putCas = putCas . Blob . encode
--     casType _ = CBlob


-- getOneSong :: Query Rows UUID (Text, Text, Maybe Text)
-- getOneSong = "select title, artist, comment from songs where id=?"

-- executeSchemaSource
