
{-# LANGUAGE OverloadedStrings #-}

module ChatTest 
  ( test
  ) where 

import ActionTest 
import ChannelsTest

import qualified Data.Text as T

test :: IO ()
test = request (CreateMessage testChannelId "Now refactoring.. ") >>= print 

