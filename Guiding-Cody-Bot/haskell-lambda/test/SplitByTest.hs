{-# LANGUAGE DeriveDataTypeable #-}


module SplitByTest where

import qualified Data.Text as T 
import Data.List.Split

command = T.pack "/guiding-codey/command"
scheduled = T.pack "/guiding-codey/scheduled/morning"
endpoint1 = "/aaaa/bbbb/cccc/dddd"
endpoint2 = "/aaaa/eeee"
endpoint3 = "/guiding-codey/scheduled/good-morning"
    
splitUrl :: String -> String
splitUrl txt = head . splitOn "/" $ tail txt
        

-- test :: IO () 
-- test = do
--     case of 

mkInput :: String -> [String]
mkInput = tail . splitOn "/"



search :: [String] -> IO () 
search [] = return ()
search ("aaaa":xs) = search xs 
search ("bbbb":xs) = search xs 
search ("cccc":xs) = search xs 
search ("dddd":xs) = putStrLn "This is endpoint1"
search ("eeee":xs) = putStrLn "This is endpoint2"
search ("ffff":xs) = search xs 
search ("gggg":xs) = putStrLn "This is endpoint3"