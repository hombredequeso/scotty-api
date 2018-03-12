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

instance ToJSON JsonErrorBody where
    toEncoding = genericToEncoding defaultOptions

getInfo :: ActionM ()
getInfo = text "info about the api" *> status badRequest400

-- Which is the same as:
--
getInfoDoEdition :: ActionM ()
getInfoDoEdition = do
    text "Here's some info about the api, or not"
    status badRequest400

resp :: ToJSON a =>
    Status
     -> a
     -> ActionM ()
resp code entity = Web.Scotty.json entity *> status code

-- Note this type definition:   type ActionM = ActionT Text IO
-- and this:                    param :: Parsable a => Text -> ActionM a
getStream :: ActionM ()
getStream = param "name"                                        -- Got an ActionM string, upwrap the ActionM monad:
                >>= (\streamStr -> return (getTransportMessage <$> streamName streamStr))                      
                >>= either                                      -- Got an ActionM (Either StreamNameError TransportMessage)
                        ( resp badRequest400 . toErrorBody )     -- finally using json/status in the resp function
                        ( resp ok200 )                          --      to turn into ActionM ()

main = scotty 3000 $ do
    get "/api/info" getInfo
    get "/api/eventstream/:name" $ do
        strName <- param "name"
        let transportMsg = getTransportMessage <$> streamName strName
        either 
            ( resp badRequest400 .toErrorBody ) 
            ( resp ok200 )  
            transportMsg

    get "/api/eventstreamBindEdition/:name" getStream
