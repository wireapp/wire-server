{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserStore.Migration.Types where

import Cassandra.Util
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.Time
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports
import Wire.API.Password
import Wire.API.User
import Wire.API.User.RichInfo

data RawUserData = RawUserData
  { id :: UserId,
    user :: UserRowCass,
    richInfo :: Maybe RichInfoAssocList,
    serviceConv :: Maybe ServiceConv,
    handleClaimValidity :: HandleClaimValidity
  }

data HandleClaimValidity
  = HandleClaimValid
  | HandleNotClaimed
  | HandleClaimedByAnotherUser UserId

-- | Some fields are read as 'Maybe' even if they're supposed to always be
-- there. This is to deal with potential old data in the DB.
data UserRowCass = UserRowCass
  { accentId :: Maybe ColourId,
    activated :: Maybe Bool,
    country :: Maybe Country,
    email :: Maybe EmailAddress,
    emailUnvalidated :: Maybe EmailAddress,
    expires :: Maybe UTCTimeMillis,
    featureConferenceCalling :: Maybe Int32,
    handle :: Maybe Handle,
    language :: Maybe Language,
    managedBy :: Maybe ManagedBy,
    name :: Maybe Name,
    password :: Maybe Password,
    providerId :: Maybe ProviderId,
    searchable :: Maybe Bool,
    serviceId :: Maybe ServiceId,
    ssoId :: Maybe UserSSOId,
    status :: Maybe AccountStatus,
    supportedProtocols :: Maybe (Set BaseProtocolTag),
    teamId :: Maybe TeamId,
    textStatus :: Maybe TextStatus,
    userType :: Maybe UserType,
    assets :: Maybe [Asset],
    pict :: Maybe Pict,
    activatedWriteTime :: Maybe (Writetime ())
  }

data ServiceConv = ServiceConv
  { convId :: ConvId,
    teamId :: Maybe TeamId
  }

data UserRowPG = UserRowPG
  { id_ :: UserId,
    accentId :: ColourId,
    activated :: Bool,
    country :: Maybe Country,
    email :: Maybe EmailAddress,
    emailUnvalidated :: Maybe EmailAddress,
    expires :: Maybe UTCTimeMillis,
    featureConferenceCalling :: Maybe Int32,
    handle :: Maybe Handle,
    language :: Maybe Language,
    managedBy :: Maybe ManagedBy,
    name :: Name,
    password :: Maybe Password,
    providerId :: Maybe ProviderId,
    searchable :: Maybe Bool,
    serviceId :: Maybe ServiceId,
    ssoId :: Maybe UserSSOId,
    status :: Maybe AccountStatus,
    supportedProtocols :: Maybe (Set BaseProtocolTag),
    teamId :: Maybe TeamId,
    textStatus :: Maybe TextStatus,
    userType :: UserType,
    assets :: Maybe [Asset],
    pict :: Maybe Pict,
    richInfo :: Maybe RichInfoAssocList,
    createdAt :: UTCTime
  }

recordInstance ''UserRowCass

recordInstance ''ServiceConv
