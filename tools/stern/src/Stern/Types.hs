{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Stern.Types where

import Control.Lens ((?~))
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Conversion
import Data.Json.Util
import Data.OpenApi qualified as Swagger
import Data.Proxy
import Data.Range
import Data.Schema qualified as S
import Imports
import Servant.API
import Wire.API.Properties
import Wire.API.Routes.Internal.Galley.TeamsIntra (TeamData)
import Wire.API.Team.Member
import Wire.API.Team.Permission

newtype TeamMemberInfo = TeamMemberInfo {tm :: TeamMember}
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema TeamMemberInfo

instance S.ToSchema TeamMemberInfo where
  schema =
    S.object "TeamMemberInfo" $
      TeamMemberInfo
        <$> tm S..= teamMemberObjectSchema
        <* ((`hasPermission` SetBilling) . tm) S..= S.field "can_update_billing" S.schema
        <* ((`hasPermission` GetBilling) . tm) S..= S.field "can_view_billing" S.schema

data TeamInfo = TeamInfo
  { tiData :: TeamData,
    tiMembers :: [TeamMemberInfo]
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema TeamInfo

instance S.ToSchema TeamInfo where
  schema =
    S.object "TeamInfo" $
      TeamInfo
        <$> tiData S..= S.field "info" S.schema
        <*> tiMembers S..= S.field "members" (S.array S.schema)

data TeamAdminInfo = TeamAdminInfo
  { taData :: TeamData,
    taOwners :: [TeamMemberInfo],
    taAdmins :: [TeamMemberInfo],
    taMembers :: Int
  }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema TeamAdminInfo

instance S.ToSchema TeamAdminInfo where
  schema =
    S.object "TeamAdminInfo" $
      TeamAdminInfo
        <$> taData S..= S.field "data" S.schema
        <*> taOwners S..= S.field "owners" (S.array S.schema)
        <*> taAdmins S..= S.field "admins" (S.array S.schema)
        <*> taMembers S..= S.field "total_members" S.schema

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
isAdmin m = hasPermission m AddTeamMember && not (hasPermission m SetBilling)

newtype UserProperties = UserProperties
  { unUserProperties :: Map PropertyKey Value
  }
  deriving (ToJSON)

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

instance Swagger.ToSchema ConsentLog where
  declareNamedSchema _ =
    pure . Swagger.NamedSchema (Just "ConsentLog") $
      mempty
        & Swagger.type_ ?~ Swagger.OpenApiObject
        & Swagger.description ?~ "(object structure is not specified in this schema)"

newtype ConsentValue = ConsentValue
  { unConsentValue :: Object
  }
  deriving (Eq, Show, ToJSON, FromJSON)

newtype MarketoResult = MarketoResult
  { unMarketoResult :: Object
  }
  deriving (Eq, Show, ToJSON, FromJSON)

data ConsentLogAndMarketo = ConsentLogAndMarketo
  { consentLog :: ConsentLog,
    marketo :: MarketoResult
  }
  deriving (Eq, Show)

deriveJSON toJSONFieldName ''ConsentLogAndMarketo

instance Swagger.ToSchema ConsentLogAndMarketo where
  declareNamedSchema _ =
    pure . Swagger.NamedSchema (Just "ConsentLogAndMarketo") $
      mempty
        & Swagger.type_ ?~ Swagger.OpenApiObject
        & Swagger.description ?~ "(object structure is not specified in this schema)"

newtype UserMetaInfo = UserMetaInfo
  { unUserMetaInfo :: Object
  }
  deriving (Eq, Show, ToJSON, FromJSON)

instance Swagger.ToSchema UserMetaInfo where
  declareNamedSchema _ =
    pure . Swagger.NamedSchema (Just "UserMetaInfo") $
      mempty
        & Swagger.type_ ?~ Swagger.OpenApiObject
        & Swagger.description ?~ "(object structure is not specified in this schema)"

newtype InvoiceId = InvoiceId {unInvoiceId :: Text}
  deriving (Eq, Show, ToByteString, FromByteString, ToJSON, FromJSON)

instance Swagger.ToParamSchema InvoiceId where
  toParamSchema _ = Swagger.toParamSchema (Proxy @Text)

instance FromHttpApiData InvoiceId where
  parseUrlPiece = fmap InvoiceId . parseUrlPiece

instance ToHttpApiData InvoiceId where
  toUrlPiece (InvoiceId t) = toUrlPiece t

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
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema TeamBillingInfo

instance S.ToSchema TeamBillingInfo where
  schema =
    S.object "TeamBillingInfo" $
      TeamBillingInfo
        <$> tbiFirstname S..= S.field "firstname" S.schema
        <*> tbiLastname S..= S.field "lastname" S.schema
        <*> tbiStreet S..= S.field "street" S.schema
        <*> tbiZip S..= S.field "zip" S.schema
        <*> tbiCity S..= S.field "city" S.schema
        <*> tbiCountry S..= S.field "country" S.schema
        <*> tbiCompany S..= S.maybe_ (S.optField "company" S.schema)
        <*> tbiState S..= S.maybe_ (S.optField "state" S.schema)

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
  deriving (ToJSON, FromJSON, Swagger.ToSchema) via S.Schema TeamBillingInfoUpdate

instance S.ToSchema TeamBillingInfoUpdate where
  schema =
    S.object "TeamBillingInfoUpdate" $
      TeamBillingInfoUpdate
        <$> tbiuFirstname S..= tbiuField "firstname"
        <*> tbiuLastname S..= tbiuField "lastname"
        <*> tbiuStreet S..= tbiuField "street"
        <*> tbiuZip S..= tbiuField "zip"
        <*> tbiuCity S..= tbiuField "city"
        <*> tbiuCountry S..= tbiuField "country"
        <*> tbiuCompany S..= tbiuField "company"
        <*> tbiuState S..= tbiuField "state"
    where
      tbiuField fnm = S.maybe_ (S.optField fnm S.schema)
