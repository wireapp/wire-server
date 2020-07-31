{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Stern.Types where

import Brig.Types
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Conversion
import qualified Data.HashMap.Strict as M
import Data.Json.Util
import Data.Range
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Imports

newtype TeamMemberInfo = TeamMemberInfo {tm :: TeamMember}

instance ToJSON TeamMemberInfo where
  toJSON (TeamMemberInfo m) =
    case teamMemberJson (const True) m of
      Object o ->
        Object $
          M.insert "can_update_billing" (Bool (hasPermission m SetBilling)) $
            M.insert "can_view_billing" (Bool (hasPermission m GetBilling)) $
              o
      other ->
        error $ "toJSON TeamMemberInfo: not an object: " <> show (encode other)

data TeamInfo = TeamInfo
  { tiData :: TeamData,
    tiMembers :: [TeamMemberInfo]
  }

data TeamAdminInfo = TeamAdminInfo
  { taData :: TeamData,
    taOwners :: [TeamMemberInfo],
    taAdmins :: [TeamMemberInfo],
    taMembers :: Int
  }

toAdminInfo :: TeamInfo -> TeamAdminInfo
toAdminInfo (TeamInfo d members) =
  TeamAdminInfo
    { taData = d,
      taMembers = length members,
      taOwners = filter (\(TeamMemberInfo m) -> isOwner m) members,
      taAdmins = filter (\(TeamMemberInfo m) -> isAdmin m) members
    }

-- FUTUREWORK: use the same criteria as in RoleOwner, RoleAdmin
isOwner :: TeamMember -> Bool
isOwner m = hasPermission m SetBilling

isAdmin :: TeamMember -> Bool
isAdmin m = (hasPermission m AddTeamMember) && not (hasPermission m SetBilling)

instance ToJSON TeamInfo where
  toJSON (TeamInfo d m) =
    object
      [ "info" .= d,
        "members" .= m
      ]

instance ToJSON TeamAdminInfo where
  toJSON (TeamAdminInfo d o a m) =
    object
      [ "info" .= d,
        "owners" .= o,
        "admins" .= a,
        "total_members" .= m
      ]

newtype UserProperties = UserProperties
  { unUserProperties :: M.HashMap PropertyKey PropertyValue
  }
  deriving (Eq, Show, ToJSON)

-- | NOTE: The following datatypes are defined by services used only internally at Wire
-- related to billing services and others and are not relevant for generic wire-server
-- installations.
--
-- For reference, these models are defined here (note that these are private repos)
-- https://github.com/zinfra/galeb/blob/develop/src/main/java/com/wire/galeb/models/ConsentLog.java
-- https://github.com/zinfra/galeb/blob/develop/src/main/java/com/wire/galeb/models/ConsentResult.java
-- https://github.com/zinfra/galeb/blob/develop/src/main/java/com/wire/galeb/models/MarketoResult.java
-- Note that we define them simply as JSON objects since we use them as a read-only and all info is to
-- be displayed to clients. Thus, it seems harmless (and easier) to just consume the whole object and
-- simply use whatever galeb's JSON object looks like
newtype ConsentLog = ConsentLog
  { unConsentLog :: Object
  }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype ConsentValue = ConsentValue
  { unConsentValue :: Object
  }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype MarketoResult = MarketoResult
  { unMarketoResult :: Object
  }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype InvoiceId = InvoiceId {unInvoiceId :: Text}
  deriving (Eq, Show, ToByteString, FromByteString, ToJSON, FromJSON)

data TeamBillingInfo = TeamBillingInfo
  { tbiFirstname :: Text,
    tbiLastname :: Text,
    tbiStreet :: Text,
    tbiZip :: Text,
    tbiCity :: Text,
    tbiCountry :: Text,
    tbiCompany :: Maybe Text,
    tbiState :: Maybe Text
  }
  deriving (Eq, Show)

deriveJSON toJSONFieldName ''TeamBillingInfo

data TeamBillingInfoUpdate = TeamBillingInfoUpdate
  { tbiuFirstname :: Maybe (Range 1 256 Text),
    tbiuLastname :: Maybe (Range 1 256 Text),
    tbiuStreet :: Maybe (Range 1 256 Text),
    tbiuZip :: Maybe (Range 1 16 Text),
    tbiuCity :: Maybe (Range 1 256 Text),
    tbiuCountry :: Maybe (Range 1 256 Text),
    tbiuCompany :: Maybe (Range 1 256 Text),
    tbiuState :: Maybe (Range 1 256 Text)
  }
  deriving (Eq, Show)

deriveJSON toJSONFieldName ''TeamBillingInfoUpdate
