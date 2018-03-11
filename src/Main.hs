{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import GHC.Generics
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Aeson hiding (json)
import Network.HTTP.Types.Status

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

sampleMessages = [
                Message{ mType = "type1", description = "some message" },
                Message { mType = "type2", description = "another message"}
           ]

getTransportMessage :: StreamName -> TransportMessage
getTransportMessage streamName =
    TransportMessage {
        Main.header = TransportMessageHeader {messageStream = streamName},
        messages = sampleMessages
    }

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

main = scotty 3000 $ do
    get "/api/eventStream/:name" $ do
        strName <- param "name"
        let strm = streamName strName
        let transportMsg = getTransportMessage <$> strm
        either (\l -> status badRequest400) (\r -> Web.Scotty.json r) transportMsg
