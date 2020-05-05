{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | > docs/reference/user/connection.md {#RefConnection}
--
-- Types for connections between users.
module Wire.API.Connection
  ( -- * UserConnection
    UserConnection (..),
    UserConnectionList (..),
    Message (..),
    Relation (..),

    -- * Requests
    ConnectionRequest (..),
    ConnectionUpdate (..),

    -- * Swagger
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util
import Data.Range
import Data.Text as Text
import Imports
import Wire.API.User.Profile ()

--------------------------------------------------------------------------------
-- UserConnection

-- | Exact state of the connection between two users, stored in Brig database (see
-- 'Brig.Data.Connection.lookupConnections').
--
-- Connection states have a direction -- e.g. if A sends a connection request to B, we'll
-- create connections (A, B, Sent) and (B, A, Pending).
data UserConnection = UserConnection
  { ucFrom :: !UserId,
    ucTo :: !UserId,
    ucStatus :: !Relation,
    -- | When 'ucStatus' was last changed
    ucLastUpdate :: !UTCTimeMillis,
    ucMessage :: !(Maybe Message),
    ucConvId :: !(Maybe ConvId)
  }
  deriving (Eq, Show, Generic)

instance ToJSON UserConnection where
  toJSON uc =
    object
      [ "from" .= ucFrom uc,
        "to" .= ucTo uc,
        "status" .= ucStatus uc,
        "last_update" .= ucLastUpdate uc,
        "message" .= ucMessage uc,
        "conversation" .= ucConvId uc
      ]

instance FromJSON UserConnection where
  parseJSON = withObject "user-connection" $ \o ->
    UserConnection <$> o .: "from"
      <*> o .: "to"
      <*> o .: "status"
      <*> o .: "last_update"
      <*> o .:? "message"
      <*> o .:? "conversation"

-- | Possible relations between two users. For detailed descriptions of these states, see:
--
-- > docs/reference/user/connection.md {#RefConnectionStates}
data Relation
  = Accepted
  | Blocked
  | Pending
  | Ignored
  | Sent
  | Cancelled
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Relation where
  toJSON = String . Text.toLower . pack . show

instance FromJSON Relation where
  parseJSON (String "accepted") = return Accepted
  parseJSON (String "blocked") = return Blocked
  parseJSON (String "pending") = return Pending
  parseJSON (String "ignored") = return Ignored
  parseJSON (String "sent") = return Sent
  parseJSON (String "cancelled") = return Cancelled
  parseJSON _ = mzero

instance FromByteString Relation where
  parser = takeByteString >>= \b -> case b of
    "accepted" -> return Accepted
    "blocked" -> return Blocked
    "pending" -> return Pending
    "ignored" -> return Ignored
    "sent" -> return Sent
    "cancelled" -> return Cancelled
    x -> fail $ "Invalid relation-type " <> show x

-- | Initial message sent along with a connection request. 1-256 characters.
--
-- /Note 2019-03-28:/ some clients send it, but we have hidden it anyway in the UI since it
-- works as a nice source of spam. TODO deprecate and remove.
newtype Message = Message {messageText :: Text}
  deriving (Eq, Ord, Show, ToJSON, Generic)

instance FromJSON Message where
  parseJSON x = Message . fromRange <$> (parseJSON x :: Parser (Range 1 256 Text))

-- | Response type for endpoints returning lists of connections.
data UserConnectionList = UserConnectionList
  { clConnections :: [UserConnection],
    -- | Pagination flag ("we have more results")
    clHasMore :: !Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON UserConnectionList where
  toJSON (UserConnectionList l m) =
    object
      [ "connections" .= l,
        "has_more" .= m
      ]

instance FromJSON UserConnectionList where
  parseJSON = withObject "UserConnectionList" $ \o ->
    UserConnectionList <$> o .: "connections"
      <*> o .: "has_more"

--------------------------------------------------------------------------------
-- Requests

-- | Payload type for a connection request from one user to another.
data ConnectionRequest = ConnectionRequest
  { -- | Connection recipient
    crUser :: !OpaqueUserId,
    -- | Name of the conversation to be created
    crName :: !Text,
    -- | Initial message
    crMessage :: !Message
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConnectionRequest where
  toJSON c =
    object
      [ "user" .= crUser c,
        "name" .= crName c,
        "message" .= crMessage c
      ]

instance FromJSON ConnectionRequest where
  parseJSON = withObject "connection-request" $ \o ->
    ConnectionRequest <$> o .: "user"
      <*> (fromRange <$> ((o .: "name") :: Parser (Range 1 256 Text)))
      <*> o .: "message"

-- | Payload type for "please change the status of this connection".
data ConnectionUpdate = ConnectionUpdate
  { cuStatus :: !Relation
  }
  deriving (Eq, Show, Generic)

instance ToJSON ConnectionUpdate where
  toJSON c = object ["status" .= cuStatus c]

instance FromJSON ConnectionUpdate where
  parseJSON = withObject "connection-update" $ \o ->
    ConnectionUpdate <$> o .: "status"
