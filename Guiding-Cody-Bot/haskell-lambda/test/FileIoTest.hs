


module FileIoTest where 


import Data.ByteString
import System.IO hiding (hGetContents)


test :: IO ByteString
test = do 
    handle <- openFile "./resource/sticker.png" ReadMode
    hGetContents handle
    
