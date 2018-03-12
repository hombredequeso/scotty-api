{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Web.Scotty
import Data.Aeson (ToJSON)
import Network.HTTP.Types.Status

import ApiTypes
import InMemoryData

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
resp code entity = json entity *> status code

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
