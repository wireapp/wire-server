{-# LANGUAGE LambdaCase #-}
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
    ConnectionsPage,
    Relation (..),
    RelationWithHistory (..),
    relationDropHistory,

    -- * Requests
    ConnectionRequest (..),
    ConnectionUpdate (..),
    ListConnectionsRequestPaginated,

    -- * Swagger
    modelConnectionList,
    modelConnection,
    modelConnectionUpdate,
  )
where

import Control.Applicative (optional)
import Control.Lens ((?~))
import Data.Aeson as Aeson
import Data.Attoparsec.ByteString (takeByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Qualified (Qualified (qUnqualified), deprecatedSchema)
import Data.Range
import qualified Data.Schema as P
import qualified Data.Swagger.Build.Api as Doc
import Data.Swagger.Schema as S
import Data.Text as Text
import Imports
import Wire.API.Arbitrary (Arbitrary (..), GenericUniform (..))
import Wire.API.Routes.MultiTablePaging

--------------------------------------------------------------------------------
-- UserConnectionList

-- | Request to get a paginated list of connection
type ListConnectionsRequestPaginated = GetMultiTablePageRequest "Connections" LocalOrRemoteTable 500 100

-- | A page in response to 'ListConnectionsRequestPaginated'
type ConnectionsPage = MultiTablePage "Connections" "connections" LocalOrRemoteTable UserConnection

-- | Response type for endpoints returning lists of connections.
data UserConnectionList = UserConnectionList
  { clConnections :: [UserConnection],
    -- | Pagination flag ("we have more results")
    clHasMore :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserConnectionList)
  deriving (FromJSON, ToJSON, S.ToSchema) via (P.Schema UserConnectionList)

instance P.ToSchema UserConnectionList where
  schema =
    P.object "UserConnectionList" $
      UserConnectionList
        <$> clConnections P..= P.field "connections" (P.array P.schema)
        <*> clHasMore P..= P.fieldWithDocModifier "has_more" (P.description ?~ "Indicator that the server has more connections than returned.") P.schema

modelConnectionList :: Doc.Model
modelConnectionList = Doc.defineModel "UserConnectionList" $ do
  Doc.description "A list of user connections."
  Doc.property "connections" (Doc.unique $ Doc.array (Doc.ref modelConnection)) Doc.end
  Doc.property "has_more" Doc.bool' $
    Doc.description "Indicator that the server has more connections than returned."

--------------------------------------------------------------------------------
-- UserConnection

-- | Exact state of the connection between two users, stored in Brig database (see
-- 'Brig.Data.Connection.lookupConnections').
--
-- Connection states have a direction -- e.g. if A sends a connection request to B, we'll
-- create connections (A, B, Sent) and (B, A, Pending).
data UserConnection = UserConnection
  { ucFrom :: UserId,
    ucTo :: Qualified UserId,
    ucStatus :: Relation,
    -- | When 'ucStatus' was last changed
    ucLastUpdate :: UTCTimeMillis,
    -- TODO: Make this qualified
    ucConvId :: Maybe ConvId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserConnection)
  deriving (FromJSON, ToJSON, S.ToSchema) via (P.Schema UserConnection)

instance P.ToSchema UserConnection where
  schema =
    P.object "UserConnection" $
      UserConnection
        <$> ucFrom P..= P.field "from" P.schema
        <*> ucTo P..= P.field "qualified_to" P.schema
        <* (qUnqualified . ucTo)
          P..= optional (P.field "to" (deprecatedSchema "qualified_to" P.schema))
        <*> ucStatus P..= P.field "status" P.schema
        <*> ucLastUpdate P..= P.field "last_update" P.schema
        <*> ucConvId P..= P.optField "conversation" Nothing P.schema

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

--------------------------------------------------------------------------------
-- Relation

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
  | -- | behaves like blocked, the extra constructor is just to inform why.
    MissingLegalholdConsent
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Relation)
  deriving (FromJSON, ToJSON, S.ToSchema) via (P.Schema Relation)

-- | 'updateConnectionInternal', requires knowledge of the previous state (before
-- 'MissingLegalholdConsent'), but the clients don't need that information.  To avoid having
-- to change the API, we introduce an internal variant of 'Relation' with surjective mapping
-- 'relationDropHistory'.
data RelationWithHistory
  = AcceptedWithHistory
  | BlockedWithHistory
  | PendingWithHistory
  | IgnoredWithHistory
  | SentWithHistory
  | CancelledWithHistory
  | MissingLegalholdConsentFromAccepted
  | MissingLegalholdConsentFromBlocked
  | MissingLegalholdConsentFromPending
  | MissingLegalholdConsentFromIgnored
  | MissingLegalholdConsentFromSent
  | MissingLegalholdConsentFromCancelled
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform RelationWithHistory)

relationDropHistory :: RelationWithHistory -> Relation
relationDropHistory = \case
  AcceptedWithHistory -> Accepted
  BlockedWithHistory -> Blocked
  PendingWithHistory -> Pending
  IgnoredWithHistory -> Ignored
  SentWithHistory -> Sent
  CancelledWithHistory -> Cancelled
  MissingLegalholdConsentFromAccepted -> MissingLegalholdConsent
  MissingLegalholdConsentFromBlocked -> MissingLegalholdConsent
  MissingLegalholdConsentFromPending -> MissingLegalholdConsent
  MissingLegalholdConsentFromIgnored -> MissingLegalholdConsent
  MissingLegalholdConsentFromSent -> MissingLegalholdConsent
  MissingLegalholdConsentFromCancelled -> MissingLegalholdConsent

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

instance P.ToSchema Relation where
  schema =
    P.enum @Text "Relation" $
      mconcat
        [ P.element "accepted" Accepted,
          P.element "blocked" Blocked,
          P.element "pending" Pending,
          P.element "ignored" Ignored,
          P.element "sent" Sent,
          P.element "cancelled" Cancelled,
          P.element "missing-legalhold-consent" MissingLegalholdConsent
        ]

instance FromByteString Relation where
  parser =
    takeByteString >>= \case
      "accepted" -> return Accepted
      "blocked" -> return Blocked
      "pending" -> return Pending
      "ignored" -> return Ignored
      "sent" -> return Sent
      "cancelled" -> return Cancelled
      "missing-legalhold-consent" -> return MissingLegalholdConsent
      x -> fail $ "Invalid relation-type " <> show x

instance ToByteString Relation where
  builder = \case
    Accepted -> "accepted"
    Blocked -> "blocked"
    Pending -> "pending"
    Ignored -> "ignored"
    Sent -> "sent"
    Cancelled -> "cancelled"
    MissingLegalholdConsent -> "missing-legalhold-consent"

--------------------------------------------------------------------------------
-- Requests

-- | Payload type for a connection request from one user to another.
data ConnectionRequest = ConnectionRequest
  { -- | Connection recipient
    crUser :: UserId,
    -- | Name of the conversation to be created. This is not used in any
    -- meaningful way anymore. The clients just write the name of the target
    -- user here and it is ignored later.
    --
    -- (In the past, this was used; but due to spam, clients started ignoring
    -- it)
    crName :: Range 1 256 Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConnectionRequest)
  deriving (FromJSON, ToJSON, S.ToSchema) via (P.Schema ConnectionRequest)

instance P.ToSchema ConnectionRequest where
  schema =
    P.object "ConnectionRequest" $
      ConnectionRequest
        <$> crUser P..= P.fieldWithDocModifier "user" (P.description ?~ "user ID of the user to request a connection with") P.schema
        <*> crName P..= P.fieldWithDocModifier "name" (P.description ?~ "Name of the (pending) conversation being initiated (1 - 256) characters)") P.schema

-- | Payload type for "please change the status of this connection".
newtype ConnectionUpdate = ConnectionUpdate
  { cuStatus :: Relation
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConnectionUpdate)
  deriving (FromJSON, ToJSON, S.ToSchema) via (P.Schema ConnectionUpdate)

instance P.ToSchema ConnectionUpdate where
  schema =
    P.object "ConnectionUpdate" $
      ConnectionUpdate
        <$> cuStatus P..= P.fieldWithDocModifier "status" (P.description ?~ "New relation status") P.schema

modelConnectionUpdate :: Doc.Model
modelConnectionUpdate = Doc.defineModel "ConnectionUpdate" $ do
  Doc.description "Connection update"
  Doc.property "status" typeRelation $
    Doc.description "New relation status"
