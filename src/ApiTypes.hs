{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ApiTypes where

import GHC.Generics
import Data.Aeson 

data Message = Message {
                            mType :: String,
                            description :: String
                       } deriving Generic

data JsonErrorBody = JsonErrorBody {
                                        description :: String
                                   } deriving Generic


newtype StreamName = StreamName String deriving Generic

data StreamNameError = InvalidStreamName

toErrorBody :: StreamNameError -> JsonErrorBody
toErrorBody streamNameError = JsonErrorBody {
                                description = "Invalid Stream Name"
                              }

streamName :: String -> Either StreamNameError StreamName
streamName s    | (length s) > 2 && (length s < 20) = Right $ StreamName s
                | otherwise = Left InvalidStreamName

data TransportMessageHeader = TransportMessageHeader {
                                                        messageStream :: StreamName
                                                     } deriving Generic

data TransportMessage = TransportMessage {
                                            header :: TransportMessageHeader,
                                            messages :: [Message]
                                         } deriving Generic

-- See: 
--      https://hackage.haskell.org/package/aeson-1.3.0.0/docs/Data-Aeson.html
instance ToJSON Message where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TransportMessageHeader where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON TransportMessage where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON StreamName where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON JsonErrorBody where
    toEncoding = genericToEncoding defaultOptions

