{-# LANGUAGE DisambiguateRecordFields #-}

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

module Wire.API.Routes.Internal.Brig.Connection where

import Data.Aeson (FromJSON, ToJSON)
import Data.Id
import Data.Qualified
import Data.Schema
import Data.Swagger qualified as S
import Imports
import Wire.API.Connection

data ConnectionsStatusRequest = ConnectionsStatusRequest
  { csrFrom :: ![UserId],
    csrTo :: !(Maybe [UserId])
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ConnectionsStatusRequest)

instance ToSchema ConnectionsStatusRequest where
  schema =
    object "ConnectionsStatusRequest" $
      ConnectionsStatusRequest
        <$> csrFrom .= field "from" (array schema)
        <*> csrTo .= maybe_ (optField "to" (array schema))

data ConnectionsStatusRequestV2 = ConnectionsStatusRequestV2
  { csrv2From :: ![UserId],
    csrv2To :: !(Maybe [Qualified UserId]),
    csrv2Relation :: !(Maybe Relation)
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ConnectionsStatusRequestV2)

instance ToSchema ConnectionsStatusRequestV2 where
  schema =
    object "ConnectionsStatusRequestV2" $
      ConnectionsStatusRequestV2
        <$> csrv2From .= field "from" (array schema)
        <*> csrv2To .= maybe_ (optField "to" (array schema))
        <*> csrv2Relation .= maybe_ (optField "relation" schema)

data ConnectionStatus = ConnectionStatus
  { csFrom :: !UserId,
    csTo :: !UserId,
    csStatus :: !Relation
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ConnectionStatus)

instance ToSchema ConnectionStatus where
  schema =
    object "ConnectionStatus" $
      ConnectionStatus
        <$> csFrom .= field "from" schema
        <*> csTo .= field "to" schema
        <*> csStatus .= field "status" schema

data ConnectionStatusV2 = ConnectionStatusV2
  { csv2From :: !UserId,
    csv2To :: !(Qualified UserId),
    csv2Status :: !Relation
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ConnectionStatusV2)

instance ToSchema ConnectionStatusV2 where
  schema =
    object "ConnectionStatusV2" $
      ConnectionStatusV2
        <$> csv2From .= field "from" schema
        <*> csv2To .= field "qualified_to" schema
        <*> csv2Status .= field "status" schema
