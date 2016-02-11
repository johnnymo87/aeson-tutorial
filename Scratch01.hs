{-# LANGUAGE OverloadedStrings #-}

module Scratch01 where

import Data.Aeson
-- HM.fromList for turning a tuple to an Object
import qualified Data.HashMap.Lazy as HM
-- V.fromList for turning a list to an Array
import qualified Data.Vector as V
-- T.putStrLn for printing Text to stdout
import qualified Data.Text.Lazy.IO as T
-- T.decodeUtf8 for turning ByteString to Text
import qualified Data.Text.Lazy.Encoding as T
-- T.reverse
import qualified Data.Text as T
import Data.Maybe (fromJust)

-- T.putStrLn . T.decodeUtf8 . encode $ val
val :: Value
val = object [ "numbers" .= ([1,2,3] :: [Int])
             , "boolean" .= True
             ]
-- val = Object $ HM.fromList [
--   ("numbers", Array $ V.fromList [Number 1, Number 2, Number 3]),
--   ("boolean", Bool True) ]


revStrings :: Value -> Value
revStrings (String x) = String (T.reverse x)
revStrings (Array x)  = Array (fmap revStrings x)
revStrings (Object x) = let revPair (k, v) = (T.reverse k, revStrings v)
                        in Object . HM.fromList . map revPair . HM.toList $ x
revStrings other      = other

-- T.putStrLn . T.decodeUtf8 . revJSON . T.encodeUtf8 =<< T.getLine
-- {"numbers":[1,2,3],"names":["Jane","Artyom"]}
revJSON = encode . revStrings . fromJust . decode
