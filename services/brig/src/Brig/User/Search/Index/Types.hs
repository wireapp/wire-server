{-# LANGUAGE StrictData #-}
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

module Brig.User.Search.Index.Types where

import Brig.Types.Search
import Control.Lens (makeLenses)
import Control.Monad.Catch
import Data.Aeson
import Data.Handle (Handle)
import Data.Id
import Data.Json.Util (UTCTimeMillis (..), toUTCTimeMillis)
import Data.Text qualified as T
import Data.Text.ICU.Translit (trans, transliterate)
import Data.Time (UTCTime)
import Database.Bloodhound hiding (key)
import Database.Bloodhound.Internal.Client (DocVersion (DocVersion))
import Imports
import Wire.API.Team.Role (Role)
import Wire.API.User
import Wire.API.User.Search (Sso (..))

data IndexDocUpdateType
  = IndexUpdateIfNewerVersion
  | IndexUpdateIfSameOrNewerVersion

data IndexUpdate
  = IndexUpdateUser IndexDocUpdateType IndexUser
  | IndexUpdateUsers IndexDocUpdateType [IndexUser]
  | IndexDeleteUser UserId

-- | Represents the ES *index*, ie. the attributes of a user that is searchable in ES.  See also:
-- 'UserDoc'.
data IndexUser = IndexUser
  { _iuUserId :: UserId,
    _iuVersion :: IndexVersion,
    _iuTeam :: Maybe TeamId,
    _iuName :: Maybe Name,
    _iuHandle :: Maybe Handle,
    _iuEmail :: Maybe EmailAddress,
    _iuColourId :: Maybe ColourId,
    _iuAccountStatus :: Maybe AccountStatus,
    _iuSAMLIdP :: Maybe Text,
    _iuManagedBy :: Maybe ManagedBy,
    _iuCreatedAt :: Maybe UTCTime,
    _iuRole :: Maybe Role,
    _iuSearchVisibilityInbound :: Maybe SearchVisibilityInbound,
    _iuScimExternalId :: Maybe Text,
    _iuSso :: Maybe Sso,
    _iuEmailUnvalidated :: Maybe EmailAddress
  }

data IndexQuery r = IndexQuery Query Filter [DefaultSort]

data IndexError
  = IndexUpdateError EsError
  | IndexLookupError EsError
  | IndexError Text
  deriving (Show)

instance Exception IndexError

newtype IndexVersion = IndexVersion {docVersion :: DocVersion}

-- | Represents an ES *document*, ie. the subset of user attributes stored in ES.
-- See also 'IndexUser'.
--
-- If a user is not searchable, e.g. because the account got
-- suspended, all fields except for the user id are set to 'Nothing' and
-- consequently removed from the index.
data UserDoc = UserDoc
  { udId :: UserId,
    udTeam :: Maybe TeamId,
    udName :: Maybe Name,
    udNormalized :: Maybe Text,
    udHandle :: Maybe Handle,
    udEmail :: Maybe EmailAddress,
    udColourId :: Maybe ColourId,
    udAccountStatus :: Maybe AccountStatus,
    udSAMLIdP :: Maybe Text,
    udManagedBy :: Maybe ManagedBy,
    udCreatedAt :: Maybe UTCTimeMillis,
    udRole :: Maybe Role,
    udSearchVisibilityInbound :: Maybe SearchVisibilityInbound,
    udScimExternalId :: Maybe Text,
    udSso :: Maybe Sso,
    udEmailUnvalidated :: Maybe EmailAddress
  }
  deriving (Eq, Show)

-- Note: Keep this compatible with the FromJSON instances
-- of 'Contact' and 'TeamContact' from 'Wire.API.User.Search
instance ToJSON UserDoc where
  toJSON ud =
    object
      [ "id" .= udId ud,
        "team" .= udTeam ud,
        "name" .= udName ud,
        "normalized" .= udNormalized ud,
        "handle" .= udHandle ud,
        "email" .= udEmail ud,
        "accent_id" .= udColourId ud,
        "account_status" .= udAccountStatus ud,
        "saml_idp" .= udSAMLIdP ud,
        "managed_by" .= udManagedBy ud,
        "created_at" .= udCreatedAt ud,
        "role" .= udRole ud,
        (fromString . T.unpack $ searchVisibilityInboundFieldName) .= udSearchVisibilityInbound ud,
        "scim_external_id" .= udScimExternalId ud,
        "sso" .= udSso ud,
        "email_unvalidated" .= udEmailUnvalidated ud
      ]

instance FromJSON UserDoc where
  parseJSON = withObject "UserDoc" $ \o ->
    UserDoc
      <$> o .: "id"
      <*> o .:? "team"
      <*> o .:? "name"
      <*> o .:? "normalized"
      <*> o .:? "handle"
      <*> o .:? "email"
      <*> o .:? "accent_id"
      <*> o .:? "account_status"
      <*> o .:? "saml_idp"
      <*> o .:? "managed_by"
      <*> o .:? "created_at"
      <*> o .:? "role"
      <*> o .:? (fromString . T.unpack $ searchVisibilityInboundFieldName)
      <*> o .:? "scim_external_id"
      <*> o .:? "sso"
      <*> o .:? "email_unvalidated"

searchVisibilityInboundFieldName :: Text
searchVisibilityInboundFieldName = "search_visibility_inbound"

makeLenses ''IndexUser

mkIndexVersion :: (MonadThrow m, Integral a) => a -> m IndexVersion
mkIndexVersion i =
  if i > fromIntegral (maxBound :: Int)
    then throwM $ IndexError "Index overflow"
    else pure . IndexVersion . fromMaybe maxBound . mkDocVersion . fromIntegral $ i

mkIndexUser :: UserId -> IndexVersion -> IndexUser
mkIndexUser u v =
  IndexUser
    { _iuUserId = u,
      _iuVersion = v,
      _iuTeam = Nothing,
      _iuName = Nothing,
      _iuHandle = Nothing,
      _iuEmail = Nothing,
      _iuColourId = Nothing,
      _iuAccountStatus = Nothing,
      _iuSAMLIdP = Nothing,
      _iuManagedBy = Nothing,
      _iuCreatedAt = Nothing,
      _iuRole = Nothing,
      _iuSearchVisibilityInbound = Nothing,
      _iuScimExternalId = Nothing,
      _iuSso = Nothing,
      _iuEmailUnvalidated = Nothing
    }

indexToDoc :: IndexUser -> UserDoc
indexToDoc iu =
  UserDoc
    { udId = _iuUserId iu,
      udTeam = _iuTeam iu,
      udName = _iuName iu,
      udAccountStatus = _iuAccountStatus iu,
      udNormalized = normalized . fromName <$> _iuName iu,
      udHandle = _iuHandle iu,
      udEmail = _iuEmail iu,
      udColourId = _iuColourId iu,
      udSAMLIdP = _iuSAMLIdP iu,
      udManagedBy = _iuManagedBy iu,
      udCreatedAt = toUTCTimeMillis <$> _iuCreatedAt iu,
      udRole = _iuRole iu,
      udSearchVisibilityInbound = _iuSearchVisibilityInbound iu,
      udScimExternalId = _iuScimExternalId iu,
      udSso = _iuSso iu,
      udEmailUnvalidated = _iuEmailUnvalidated iu
    }

-- | FUTUREWORK: Transliteration should be left to ElasticSearch (ICU plugin), but this will
-- require a data migration.
normalized :: Text -> Text
normalized = transliterate (trans "Any-Latin; Latin-ASCII; Lower")

docToIndex :: UserDoc -> IndexUser
docToIndex ud =
  -- (Don't use 'mkIndexUser' here!  With 'IndexUser', you get compiler warnings if you
  -- forget to add new fields here.)
  IndexUser
    { _iuUserId = udId ud,
      _iuVersion = IndexVersion (DocVersion 1),
      _iuTeam = udTeam ud,
      _iuName = udName ud,
      _iuHandle = udHandle ud,
      _iuEmail = udEmail ud,
      _iuColourId = udColourId ud,
      _iuAccountStatus = udAccountStatus ud,
      _iuSAMLIdP = udSAMLIdP ud,
      _iuManagedBy = udManagedBy ud,
      _iuCreatedAt = fromUTCTimeMillis <$> udCreatedAt ud,
      _iuRole = udRole ud,
      _iuSearchVisibilityInbound = udSearchVisibilityInbound ud,
      _iuScimExternalId = udScimExternalId ud,
      _iuSso = udSso ud,
      _iuEmailUnvalidated = udEmailUnvalidated ud
    }
