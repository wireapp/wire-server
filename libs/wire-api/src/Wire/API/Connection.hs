{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
    ConnectionPagingState,
    pattern ConnectionPagingState,
    Relation (..),
    RelationWithHistory (..),
    relationDropHistory,
    relationWithHistory,

    -- * Requests
    ConnectionRequest (..),
    ConnectionUpdate (..),
    ListConnectionsRequestPaginated,
  )
where

import Cassandra qualified as C
import Control.Applicative (optional)
import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.OpenApi qualified as S
import Data.Qualified (Qualified (qUnqualified), Remote, deprecatedSchema)
import Data.Range
import Data.Schema
import Data.Text as Text
import Imports
import Servant.API
import Wire.API.Routes.MultiTablePaging
import Wire.Arbitrary (Arbitrary (..), GenericUniform (..))
import Wire.Sem.Paging
import Wire.Sem.Paging.Cassandra

--------------------------------------------------------------------------------
-- UserConnectionList

-- | Request to get a paginated list of connection
type ListConnectionsRequestPaginated = GetMultiTablePageRequest "Connections" LocalOrRemoteTable 500 100

-- | A page in response to 'ListConnectionsRequestPaginated'
type ConnectionsPage = MultiTablePage "Connections" "connections" LocalOrRemoteTable UserConnection

type ConnectionPagingName = "ConnectionIds"

type ConnectionPagingState = MultiTablePagingState ConnectionPagingName LocalOrRemoteTable

pattern ConnectionPagingState :: tables -> Maybe ByteString -> MultiTablePagingState name tables
pattern ConnectionPagingState table state = MultiTablePagingState table state

-- | Response type for endpoints returning lists of connections.
data UserConnectionList = UserConnectionList
  { clConnections :: [UserConnection],
    -- | Pagination flag ("we have more results")
    clHasMore :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserConnectionList)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema UserConnectionList)

instance ToSchema UserConnectionList where
  schema =
    object "UserConnectionList" $
      UserConnectionList
        <$> clConnections .= field "connections" (array schema)
        <*> clHasMore .= fieldWithDocModifier "has_more" (description ?~ "Indicator that the server has more connections than returned.") schema

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
    ucConvId :: Maybe (Qualified ConvId)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UserConnection)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema UserConnection)

instance ToSchema UserConnection where
  schema =
    object "UserConnection" $
      UserConnection
        <$> ucFrom .= field "from" schema
        <*> ucTo .= field "qualified_to" schema
        <* (qUnqualified . ucTo)
          .= optional (field "to" (deprecatedSchema "qualified_to" schema))
        <*> ucStatus .= field "status" schema
        <*> ucLastUpdate .= field "last_update" schema
        <*> ucConvId .= maybe_ (optField "qualified_conversation" schema)
        <* (fmap qUnqualified . ucConvId)
          .= maybe_ (optField "conversation" (deprecatedSchema "qualified_conversation" schema))

type instance PagingBounds InternalPaging (Remote UserConnection) = Range 1 1000 Int32

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
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Relation)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema Relation)

instance S.ToParamSchema Relation where
  toParamSchema _ = mempty & S.type_ ?~ S.OpenApiString

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

-- | Convert a 'Relation' to 'RelationWithHistory'. This is to be used only if
-- the MissingLegalholdConsent case does not need to be supported.
relationWithHistory :: Relation -> RelationWithHistory
relationWithHistory Accepted = AcceptedWithHistory
relationWithHistory Blocked = BlockedWithHistory
relationWithHistory Pending = PendingWithHistory
relationWithHistory Ignored = IgnoredWithHistory
relationWithHistory Sent = SentWithHistory
relationWithHistory Cancelled = CancelledWithHistory
relationWithHistory MissingLegalholdConsent = MissingLegalholdConsentFromCancelled

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

instance ToSchema Relation where
  schema =
    enum @Text "Relation" $
      mconcat
        [ element "accepted" Accepted,
          element "blocked" Blocked,
          element "pending" Pending,
          element "ignored" Ignored,
          element "sent" Sent,
          element "cancelled" Cancelled,
          element "missing-legalhold-consent" MissingLegalholdConsent
        ]

instance FromHttpApiData Relation where
  parseQueryParam = \case
    "accepted" -> pure Accepted
    "blocked" -> pure Blocked
    "pending" -> pure Pending
    "ignored" -> pure Ignored
    "sent" -> pure Sent
    "cancelled" -> pure Cancelled
    "missing-legalhold-consent" -> pure MissingLegalholdConsent
    x -> Left $ "Invalid relation-type " <> x

instance ToHttpApiData Relation where
  toQueryParam = \case
    Accepted -> "accepted"
    Blocked -> "blocked"
    Pending -> "pending"
    Ignored -> "ignored"
    Sent -> "sent"
    Cancelled -> "cancelled"
    MissingLegalholdConsent -> "missing-legalhold-consent"

instance C.Cql RelationWithHistory where
  ctype = C.Tagged C.IntColumn

  fromCql (C.CqlInt i) = case i of
    0 -> pure AcceptedWithHistory
    1 -> pure BlockedWithHistory
    2 -> pure PendingWithHistory
    3 -> pure IgnoredWithHistory
    4 -> pure SentWithHistory
    5 -> pure CancelledWithHistory
    6 -> pure MissingLegalholdConsentFromAccepted
    7 -> pure MissingLegalholdConsentFromBlocked
    8 -> pure MissingLegalholdConsentFromPending
    9 -> pure MissingLegalholdConsentFromIgnored
    10 -> pure MissingLegalholdConsentFromSent
    11 -> pure MissingLegalholdConsentFromCancelled
    n -> Left $ "unexpected RelationWithHistory: " ++ show n
  fromCql _ = Left "RelationWithHistory: int expected"

  toCql AcceptedWithHistory = C.CqlInt 0
  toCql BlockedWithHistory = C.CqlInt 1
  toCql PendingWithHistory = C.CqlInt 2
  toCql IgnoredWithHistory = C.CqlInt 3
  toCql SentWithHistory = C.CqlInt 4
  toCql CancelledWithHistory = C.CqlInt 5
  toCql MissingLegalholdConsentFromAccepted = C.CqlInt 6
  toCql MissingLegalholdConsentFromBlocked = C.CqlInt 7
  toCql MissingLegalholdConsentFromPending = C.CqlInt 8
  toCql MissingLegalholdConsentFromIgnored = C.CqlInt 9
  toCql MissingLegalholdConsentFromSent = C.CqlInt 10
  toCql MissingLegalholdConsentFromCancelled = C.CqlInt 11

----------------
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
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema ConnectionRequest)

instance ToSchema ConnectionRequest where
  schema =
    object "ConnectionRequest" $
      ConnectionRequest
        <$> crUser .= fieldWithDocModifier "user" (description ?~ "user ID of the user to request a connection with") schema
        <*> crName .= fieldWithDocModifier "name" (description ?~ "Name of the (pending) conversation being initiated (1 - 256) characters)") schema

-- | Payload type for "please change the status of this connection".
newtype ConnectionUpdate = ConnectionUpdate
  { cuStatus :: Relation
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConnectionUpdate)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema ConnectionUpdate)

instance ToSchema ConnectionUpdate where
  schema =
    object "ConnectionUpdate" $
      ConnectionUpdate
        <$> cuStatus .= fieldWithDocModifier "status" (description ?~ "New relation status") schema
