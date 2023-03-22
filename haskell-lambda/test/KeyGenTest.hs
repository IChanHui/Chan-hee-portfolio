
{-# LANGUAGE OverloadedStrings #-}

module KeyGenTest 
  ( verify 
  ) where 


import qualified NaCl.Sign as Sign
import qualified Data.ByteString as B 
import qualified Data.ByteArray as BA 
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T 
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Internal.Encoding.Utf8 as TEU
import Data.Word
import Text.Hex (encodeHex)


test :: IO ()
test = do
  (p, s) <- Sign.keypair
  let pk = encodeHex . BA.pack . BA.unpack $ p
  let sk = encodeHex . BA.pack . BA.unpack $ s
  print (T.append "Public Key: " pk)
  print (T.append "Secret Key: " sk)
  print $ verify "test " [pk, sk]

  
verify :: String -> [T.Text] -> Maybe B.ByteString
verify msg keys = do
  pk <- Sign.toPublicKey publicKey 
  sk <- Sign.toSecretKey secretKey 
  Sign.open pk (Sign.create sk bmsg :: B.ByteString) 

  where 
    bmsg = TE.encodeUtf8 (T.pack msg )
    (publicKey:secretKey:_) = map (encodeHexString . T.unpack) keys


encodeHexString :: String -> BC.ByteString 
encodeHexString = B.pack . hex2word

hex2word :: String -> [Word8]
hex2word [] = []
hex2word (a:b:ss) = fromInteger (h a * 16 + h b) : hex2word ss


h :: (Num a) => Char -> a
h '0' =  0
h '1' =  1
h '2' =  2
h '3' =  3
h '4' =  4
h '5' =  5
h '6' =  6
h '7' =  7
h '8' =  8
h '9' =  9 
h 'a' =  10
h 'b' =  11
h 'c' =  12 
h 'd' =  13 
h 'e' =  14
h 'f' =  15
