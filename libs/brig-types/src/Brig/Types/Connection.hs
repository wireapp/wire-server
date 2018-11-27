{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}

module Brig.Types.Connection
    ( module Brig.Types.Connection
    , module C
    ) where

import Imports
import Brig.Types.Common as C
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util
import Data.Range
import Data.Text as Text

newtype Message = Message { messageText :: Text }
    deriving (Eq, Ord, Show, ToJSON)

-- | Look at @services\/brig\/doc@ for descriptions of these states.
data Relation
    = Accepted
    | Blocked
    | Pending
    | Ignored
    | Sent
    | Cancelled
    deriving (Eq, Ord, Show)

data UserConnection = UserConnection
    { ucFrom       :: !UserId
    , ucTo         :: !UserId
    , ucStatus     :: !Relation
    , ucLastUpdate :: !UTCTimeMillis
    , ucMessage    :: !(Maybe Message)
    , ucConvId     :: !(Maybe ConvId)
    } deriving (Eq, Show)

data ConnectionRequest = ConnectionRequest
    { crUser    :: !UserId
    , crName    :: !Text   -- ^ Conversation name
    , crMessage :: !Message
    } deriving (Eq, Show)

data ConnectionUpdate = ConnectionUpdate
    { cuStatus :: !Relation
    } deriving (Eq, Show)

data UserConnectionList = UserConnectionList
    { clConnections :: [UserConnection]
    , clHasMore     :: !Bool
    } deriving (Eq, Show)

data UserIds = UserIds
    { cUsers :: [UserId] }

-- | Data that is passed to the @\/i\/users\/connections-status@ endpoint.
data ConnectionsStatusRequest = ConnectionsStatusRequest
    { csrFrom :: ![UserId]
    , csrTo   :: ![UserId]
    } deriving (Eq, Show)

-- * JSON Instances:

instance FromJSON Message where
    parseJSON x = Message . fromRange <$> (parseJSON x :: Parser (Range 1 256 Text))

instance ToJSON Relation where
    toJSON = String . Text.toLower . pack . show

instance FromJSON Relation where
    parseJSON (String "accepted")  = return Accepted
    parseJSON (String "blocked")   = return Blocked
    parseJSON (String "pending")   = return Pending
    parseJSON (String "ignored")   = return Ignored
    parseJSON (String "sent")      = return Sent
    parseJSON (String "cancelled") = return Cancelled
    parseJSON _                    = mzero

instance FromByteString Relation where
    parser = takeByteString >>= \b -> case b of
        "accepted"  -> return Accepted
        "blocked"   -> return Blocked
        "pending"   -> return Pending
        "ignored"   -> return Ignored
        "sent"      -> return Sent
        "cancelled" -> return Cancelled
        x           -> fail $ "Invalid relation-type " <> show x

instance FromJSON UserConnection where
    parseJSON = withObject "user-connection" $ \o ->
        UserConnection <$> o .:  "from"
                       <*> o .:  "to"
                       <*> o .:  "status"
                       <*> o .:  "last_update"
                       <*> o .:? "message"
                       <*> o .:? "conversation"

instance ToJSON UserConnection where
    toJSON uc = object
        [ "from"         .= ucFrom uc
        , "to"           .= ucTo uc
        , "status"       .= ucStatus uc
        , "last_update"  .= ucLastUpdate uc
        , "message"      .= ucMessage uc
        , "conversation" .= ucConvId uc
        ]

instance FromJSON ConnectionRequest where
    parseJSON = withObject "connection-request" $ \o ->
        ConnectionRequest <$> o .: "user"
                          <*> (fromRange <$> ((o .: "name") :: Parser (Range 1 256 Text)))
                          <*> o .: "message"

instance ToJSON ConnectionUpdate where
    toJSON c = object ["status" .= cuStatus c]

instance FromJSON ConnectionUpdate where
    parseJSON = withObject "connection-update" $ \o ->
        ConnectionUpdate <$> o .: "status"

instance ToJSON ConnectionRequest where
    toJSON c = object [ "user"    .= crUser c
                      , "name"    .= crName c
                      , "message" .= crMessage c
                      ]

instance ToJSON UserConnectionList where
    toJSON (UserConnectionList l m) = object
        [ "connections" .= l
        , "has_more"    .= m
        ]

instance FromJSON UserConnectionList where
    parseJSON = withObject "UserConnectionList" $ \o ->
        UserConnectionList <$> o .: "connections"
                           <*> o .: "has_more"

instance FromJSON UserIds where
    parseJSON = withObject "userids" $ \o ->
        UserIds <$> o .: "ids"

instance ToJSON UserIds where
    toJSON (UserIds us) = object
        [ "ids" .= us ]

instance FromJSON ConnectionsStatusRequest where
    parseJSON = withObject "ConnectionsStatusRequest" $ \o -> do
        csrFrom <- o .: "from"
        csrTo   <- o .: "to"
        pure ConnectionsStatusRequest{..}

instance ToJSON ConnectionsStatusRequest where
    toJSON ConnectionsStatusRequest{csrFrom, csrTo} = object
        [ "from" .= csrFrom
        , "to"   .= csrTo
        ]
