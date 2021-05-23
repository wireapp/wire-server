{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

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
    Relation_' (..),

    -- * Requests
    ConnectionRequest (..),
    ConnectionUpdate (..),

    -- * Swagger
    modelConnectionList,
    modelConnection,
    modelConnectionRequest,
    modelConnectionUpdate,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Range
import qualified Data.Swagger.Build.Api as Doc
import Data.Swagger.Schema
import Data.Text as Text
import Deriving.Swagger (CamelToKebab, ConstructorTagModifier, CustomSwagger, StripSuffix)
import Imports
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- UserConnectionList

-- | Response type for endpoints returning lists of connections.
data UserConnectionList = UserConnectionList
  { clConnections :: [UserConnection],
    -- | Pagination flag ("we have more results")
    clHasMore :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserConnectionList)

modelConnectionList :: Doc.Model
modelConnectionList = Doc.defineModel "UserConnectionList" $ do
  Doc.description "A list of user connections."
  Doc.property "connections" (Doc.unique $ Doc.array (Doc.ref modelConnection)) Doc.end
  Doc.property "has_more" Doc.bool' $
    Doc.description "Indicator that the server has more connections than returned."

instance ToJSON UserConnectionList where
  toJSON (UserConnectionList l m) =
    object
      [ "connections" .= l,
        "has_more" .= m
      ]

instance FromJSON UserConnectionList where
  parseJSON = withObject "UserConnectionList" $ \o ->
    UserConnectionList
      <$> o .: "connections"
      <*> o .: "has_more"

--------------------------------------------------------------------------------
-- UserConnection

-- | Exact state of the connection between two users, stored in Brig database (see
-- 'Brig.Data.Connection.lookupConnections').
--
-- Connection states have a direction -- e.g. if A sends a connection request to B, we'll
-- create connections (A, B, Sent) and (B, A, Pending).
data UserConnection = UserConnection
  { ucFrom :: UserId,
    ucTo :: UserId,
    ucStatus :: Relation_',
    -- | When 'ucStatus' was last changed
    ucLastUpdate :: UTCTimeMillis,
    ucMessage :: Maybe Message,
    ucConvId :: Maybe ConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserConnection)

modelConnection :: Doc.Model
modelConnection = Doc.defineModel "Connection" $ do
  Doc.description "Directed connection between two users"
  Doc.property "from" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "to" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "status" typeRelation $
    Doc.description "Relation status"
  Doc.property "last_update" Doc.dateTime' $
    Doc.description "Timestamp of last update"
  Doc.property "message" Doc.string' $ do
    Doc.description "Message"
    Doc.optional
  Doc.property "conversation" Doc.bytes' $ do
    Doc.description "Conversation ID"
    Doc.optional

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
    UserConnection
      <$> o .: "from"
      <*> o .: "to"
      <*> o .: "status"
      <*> o .: "last_update"
      <*> o .:? "message"
      <*> o .:? "conversation"

--------------------------------------------------------------------------------
-- Relation

-- | Possible relations between two users. For detailed descriptions of these states, see:
--
-- > docs/reference/user/connection.md {#RefConnectionStates}
data Relation_'
  = Accepted_'
  | Blocked_'
  | Pending_'
  | Ignored_'
  | Sent_'
  | Cancelled_'
  | -- | behaves like blocked, the extra constructor is just to inform why.
    MissingLegalholdConsent_'
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Relation_')
  deriving (ToSchema) via (CustomSwagger '[ConstructorTagModifier (CamelToKebab, StripSuffix "_'")] Relation_')

typeRelation :: Doc.DataType
typeRelation =
  Doc.string $
    Doc.enum
      [ "accepted",
        "blocked",
        "pending",
        "ignored",
        "sent",
        "cancelled",
        "missing-legalhold-consent"
      ]

instance ToJSON Relation_' where
  toJSON = \case
    Accepted_' -> "accepted"
    Blocked_' -> "blocked"
    Pending_' -> "pending"
    Ignored_' -> "ignored"
    Sent_' -> "sent"
    Cancelled_' -> "cancelled"
    MissingLegalholdConsent_' -> "missing-legalhold-consent"

instance FromJSON Relation_' where
  parseJSON (String "accepted") = return Accepted_'
  parseJSON (String "blocked") = return Blocked_'
  parseJSON (String "pending") = return Pending_'
  parseJSON (String "ignored") = return Ignored_'
  parseJSON (String "sent") = return Sent_'
  parseJSON (String "cancelled") = return Cancelled_'
  parseJSON (String "missing-legalhold-consent") = return MissingLegalholdConsent_'
  parseJSON _ = mzero

instance FromByteString Relation_' where
  parser =
    takeByteString >>= \b -> case b of
      "accepted" -> return Accepted_'
      "blocked" -> return Blocked_'
      "pending" -> return Pending_'
      "ignored" -> return Ignored_'
      "sent" -> return Sent_'
      "cancelled" -> return Cancelled_'
      "missing-legalhold-consent" -> return MissingLegalholdConsent_'
      x -> fail $ "Invalid relation-type " <> show x

instance ToByteString Relation_' where
  builder = \case
    Accepted_' -> "accepted"
    Blocked_' -> "blocked"
    Pending_' -> "pending"
    Ignored_' -> "ignored"
    Sent_' -> "sent"
    Cancelled_' -> "cancelled"
    MissingLegalholdConsent_' -> "missing-legalhold-consent"

--------------------------------------------------------------------------------
-- Message

-- | Initial message sent along with a connection request. 1-256 characters.
--
-- /Note 2019-03-28:/ some clients send it, but we have hidden it anyway in the UI since it
-- works as a nice source of spam. TODO deprecate and remove.
newtype Message = Message {messageText :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON)
  deriving (Arbitrary) via (Ranged 1 256 Text)

instance FromJSON Message where
  parseJSON x = Message . fromRange <$> (parseJSON x :: Parser (Range 1 256 Text))

--------------------------------------------------------------------------------
-- Requests

-- | Payload type for a connection request from one user to another.
data ConnectionRequest = ConnectionRequest
  { -- | Connection recipient
    crUser :: UserId,
    -- | Name of the conversation to be created
    crName :: Text,
    -- | Initial message
    crMessage :: Message
  }
  deriving stock (Eq, Show, Generic)

modelConnectionRequest :: Doc.Model
modelConnectionRequest = Doc.defineModel "ConnectionRequest" $ do
  Doc.description "Connection request from one user to another"
  Doc.property "user" Doc.bytes' $
    Doc.description "User ID of the user to request a connection with"
  Doc.property "name" Doc.string' $
    Doc.description "Name of the (pending) conversation being initiated (1 - 256 characters)."
  Doc.property "message" Doc.string' $
    Doc.description "The initial message in the request (1 - 256 characters)."

instance ToJSON ConnectionRequest where
  toJSON c =
    object
      [ "user" .= crUser c,
        "name" .= crName c,
        "message" .= crMessage c
      ]

instance FromJSON ConnectionRequest where
  parseJSON = withObject "connection-request" $ \o ->
    ConnectionRequest
      <$> o .: "user"
      <*> (fromRange <$> ((o .: "name") :: Parser (Range 1 256 Text)))
      <*> o .: "message"

-- | TODO: make 'crName :: Range 1 256 Text' and derive this instance.
instance Arbitrary ConnectionRequest where
  arbitrary =
    ConnectionRequest
      <$> arbitrary
      <*> (fromRange <$> arbitrary @(Range 1 256 Text))
      <*> arbitrary

-- | Payload type for "please change the status of this connection".
data ConnectionUpdate = ConnectionUpdate
  { cuStatus :: Relation_'
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConnectionUpdate)

modelConnectionUpdate :: Doc.Model
modelConnectionUpdate = Doc.defineModel "ConnectionUpdate" $ do
  Doc.description "Connection update"
  Doc.property "status" typeRelation $
    Doc.description "New relation status"

instance ToJSON ConnectionUpdate where
  toJSON c = object ["status" .= cuStatus c]

instance FromJSON ConnectionUpdate where
  parseJSON = withObject "connection-update" $ \o ->
    ConnectionUpdate <$> o .: "status"
