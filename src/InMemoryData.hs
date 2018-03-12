{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module InMemoryData where

import ApiTypes

sampleMessages = [
                Message{ mType = "type1", description = "some message" },
                Message { mType = "type2", description = "another message"}
           ]

getTransportMessage :: StreamName -> TransportMessage
getTransportMessage streamName =
    TransportMessage {
        header = TransportMessageHeader {messageStream = streamName},
        messages = sampleMessages
    }

