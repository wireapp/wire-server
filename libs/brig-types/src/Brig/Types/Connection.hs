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
    ConnectionsStatusRequest (..),

    -- * re-exports
    Message (..),
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
import Imports
import Wire.API.Connection

-- | Response type for endpoints returning lists of users with a specific connection state.
-- E.g. 'getContactList' returns a 'UserIds' containing the list of connections in an
-- 'Accepted' state.
data UserIds = UserIds
  {cUsers :: [UserId]}
  deriving (Eq, Show, Generic)

-- | Data that is passed to the @\/i\/users\/connections-status@ endpoint.
data ConnectionsStatusRequest = ConnectionsStatusRequest
  { csrFrom :: ![UserId],
    csrTo :: ![UserId]
  }
  deriving (Eq, Show, Generic)

----------------------------------------------------------------------------
-- JSON instances

instance FromJSON UserIds where
  parseJSON = withObject "userids" $ \o ->
    UserIds <$> o .: "ids"

instance ToJSON UserIds where
  toJSON (UserIds us) =
    object
      ["ids" .= us]

instance FromJSON ConnectionsStatusRequest where
  parseJSON = withObject "ConnectionsStatusRequest" $ \o -> do
    csrFrom <- o .: "from"
    csrTo <- o .: "to"
    pure ConnectionsStatusRequest {..}

instance ToJSON ConnectionsStatusRequest where
  toJSON ConnectionsStatusRequest {csrFrom, csrTo} =
    object
      [ "from" .= csrFrom,
        "to" .= csrTo
      ]
