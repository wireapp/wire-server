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
module Brig.Types.Connection
  ( module C,
    UserIds (..),
    UpdateConnectionsInternal (..),

    -- * re-exports
    Relation (..),
    UserConnection (..),
    ConnectionRequest (..),
    ConnectionUpdate (..),
    UserConnectionList (..),
  )
where

import Brig.Types.Common as C
import Data.Aeson
import Data.Id (UserId)
import Data.Qualified
import Imports
import Wire.API.Arbitrary
import Wire.API.Connection

-- | Response type for endpoints returning lists of users with a specific connection state.
-- E.g. 'getContactList' returns a 'UserIds' containing the list of connections in an
-- 'Accepted' state.
data UserIds = UserIds
  {cUsers :: [UserId]}
  deriving (Eq, Show, Generic)

-- FUTUREWORK: This needs to get Qualified IDs when implementing
-- Legalhold + Federation, as it's used in the internal
-- putConnectionInternal / galley->Brig "/i/users/connections-status"
-- endpoint.
-- Internal RPCs need to be updated accordingly.
-- See https://wearezeta.atlassian.net/browse/SQCORE-973
data UpdateConnectionsInternal
  = BlockForMissingLHConsent UserId [UserId]
  | RemoveLHBlocksInvolving UserId
  | -- | This must only be used by tests
    CreateConnectionForTest UserId (Qualified UserId)
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UpdateConnectionsInternal)

instance FromJSON UpdateConnectionsInternal

-- | `{"tag":"BlockForMissingLHConsent","contents":["3ae7f23a-bd47-11eb-932d-5fccbbcde454",["3ae7f23a-bd47-11eb-932d-5fccbbcde454"]]}`
instance ToJSON UpdateConnectionsInternal

----------------------------------------------------------------------------
-- JSON instances

instance FromJSON UserIds where
  parseJSON = withObject "userids" $ \o ->
    UserIds <$> o .: "ids"

instance ToJSON UserIds where
  toJSON (UserIds us) =
    object
      ["ids" .= us]
