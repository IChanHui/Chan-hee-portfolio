module MaybeFunctionTest where 


import Data.Aeson
import Data.List
import Data.Monoid
import Text.Read (readMaybe)
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as Text (null, pack, unpack)



quotMaybe :: Integer -> Integer -> Maybe Integer
quotMaybe x 0 = Nothing
quotMaybe x y = return $ quot x y

headM :: [a] -> Maybe a
headM [] = Nothing 
headM (x:xs) = Just x


tailM :: [a] -> Maybe [a]
tailM [] = Nothing 
tailM (x:xs) = Just xs


lastM :: [a] -> Maybe a
lastM [] = Nothing 
lastM (x:[]) = Just x
lastM (_:xs) = lastM xs

initM :: [a] -> Maybe [a]
initM [] = Nothing 
initM (x:[]) = Just []
initM (x:xs) = (x:) <$> initM xs



maybeInt :: Maybe Integer -> Integer
maybeInt Nothing = -1
maybeInt (Just value) = value

maybeToText :: Maybe Text -> Text
maybeToText Nothing = Text.pack " "
maybeToText (Just value) = value




