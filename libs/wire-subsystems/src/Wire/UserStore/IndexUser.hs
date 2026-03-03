{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserStore.IndexUser where

import Cassandra.Util
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Text.ICU.Translit
import Data.Time
import Database.CQL.Protocol
import Imports
import SAML2.WebSSO qualified as SAML
import URI.ByteString
import Wire.API.Team.Role (Role)
import Wire.API.User hiding (userId)
import Wire.API.User.Search
import Wire.UserSearch.Types

type Activated = Bool

data WithWritetime a = WithWriteTime {value :: a, writetime :: Writetime a}
  deriving (Eq, Show)

data IndexUser = IndexUser
  { userId :: UserId,
    userType :: UserType,
    teamId :: Maybe TeamId,
    name :: Name,
    accountStatus :: Maybe AccountStatus,
    handle :: Maybe Handle,
    email :: Maybe EmailAddress,
    colourId :: ColourId,
    activated :: Activated,
    serviceId :: Maybe ServiceId,
    managedBy :: Maybe ManagedBy,
    ssoId :: Maybe UserSSOId,
    unverifiedEmail :: Maybe EmailAddress,
    searchable :: Maybe Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show)

{- ORMOLU_DISABLE -}
type instance
  TupleType IndexUser =
    ( UserId,
      UserType,
      Maybe TeamId, Maybe (Writetime TeamId),
      Name, Writetime Name,
      Maybe AccountStatus, Maybe (Writetime AccountStatus),
      Maybe Handle, Maybe (Writetime Handle),
      Maybe EmailAddress, Maybe (Writetime EmailAddress),
      ColourId, Writetime ColourId,
      Activated, Writetime Activated,
      Maybe ServiceId, Maybe (Writetime ServiceId),
      Maybe ManagedBy, Maybe (Writetime ManagedBy),
      Maybe UserSSOId, Maybe (Writetime UserSSOId),
      Maybe EmailAddress, Maybe (Writetime EmailAddress),
      Maybe Bool, Maybe (Writetime Bool),
      Maybe (Writetime WriteTimeBumper)
    )

indexUserFromTuple :: TupleType IndexUser -> IndexUser
indexUserFromTuple
    ( userId,
      userType,
      teamId, tTeam,
      name, tName,
      accountStatus, tStatus,
      handle, tHandle,
      email, tEmail,
      colourId, tColour,
      activated, tActivated,
      serviceId, tService,
      managedBy, tManagedBy,
      ssoId, tSsoId,
      unverifiedEmail, tEmailUnvalidated,
      searchable, tSearchable,
      tWriteTimeBumper
    ) = IndexUser {
          createdAt = writetimeToUTC tActivated,
          updatedAt = maximum $ catMaybes [writetimeToUTC <$> tTeam,
                                           Just $ writetimeToUTC  tName,
                                           writetimeToUTC <$> tStatus,
                                           writetimeToUTC <$> tHandle,
                                           writetimeToUTC <$> tEmail,
                                           Just $ writetimeToUTC tColour,
                                           Just $ writetimeToUTC tActivated,
                                           writetimeToUTC <$> tService,
                                           writetimeToUTC <$> tManagedBy,
                                           writetimeToUTC <$> tSsoId,
                                           writetimeToUTC <$> tEmailUnvalidated,
                                           writetimeToUTC <$> tSearchable,
                                           writetimeToUTC <$> tWriteTimeBumper
                                          ],
            ..
        }
{- ORMOLU_ENABLE -}

indexUserToVersion :: Maybe (WithWritetime Role) -> IndexUser -> IndexVersion
indexUserToVersion role iu =
  mkIndexVersion [Just $ Writetime iu.updatedAt, const () <$$> fmap writetime role]

indexUserToDoc :: SearchVisibilityInbound -> Maybe Role -> IndexUser -> UserDoc
indexUserToDoc searchVisInbound mRole IndexUser {..} =
  if shouldIndex
    then
      UserDoc
        { udId = userId,
          udType = Just userType,
          udSearchable = searchable,
          udEmailUnvalidated = unverifiedEmail,
          udSso = sso =<< ssoId,
          udScimExternalId = join $ scimExternalId <$> (managedBy) <*> (ssoId),
          udSearchVisibilityInbound = Just searchVisInbound,
          udRole = mRole,
          udCreatedAt = Just . toUTCTimeMillis $ createdAt,
          udManagedBy = managedBy,
          udSAMLIdP = idpUrl =<< ssoId,
          udAccountStatus = accountStatus,
          udColourId = Just colourId,
          udEmail = email,
          udHandle = handle,
          udNormalized = Just $ normalized name.fromName,
          udName = Just name,
          udTeam = teamId
        }
    else -- We insert a tombstone-style user here, as it's easier than
    -- deleting the old one. It's mostly empty, but having the status here
    -- might be useful in the future.
      emptyUserDoc userId
  where
    shouldIndex =
      ( case accountStatus of
          Nothing -> True
          Just Active -> True
          Just Suspended -> True
          Just Deleted -> False
          Just Ephemeral -> False
          Just PendingInvitation -> False
      )
        && activated -- FUTUREWORK: how is this adding to the first case?
        && isNothing serviceId

    idpUrl :: UserSSOId -> Maybe Text
    idpUrl (UserSSOId (SAML.UserRef (SAML.Issuer uri) _subject)) =
      Just $ fromUri uri
    idpUrl (UserScimExternalId _) = Nothing

    fromUri :: URI -> Text
    fromUri =
      Text.decodeUtf8With Text.lenientDecode
        . LBS.toStrict
        . toLazyByteString
        . serializeURIRef

    sso :: UserSSOId -> Maybe Sso
    sso userSsoId = do
      (issuer, nameid) <- ssoIssuerAndNameId userSsoId
      pure $ Sso {ssoIssuer = issuer, ssoNameId = nameid}

-- Transliteration could also be done by ElasticSearch (ICU plugin), but this would
-- require a data migration.
normalized :: Text -> Text
normalized = transliterate (trans "Any-Latin; Latin-ASCII; Lower")

emptyUserDoc :: UserId -> UserDoc
emptyUserDoc uid =
  UserDoc
    { udType = Nothing,
      udSearchable = Nothing,
      udEmailUnvalidated = Nothing,
      udSso = Nothing,
      udScimExternalId = Nothing,
      udSearchVisibilityInbound = Nothing,
      udRole = Nothing,
      udCreatedAt = Nothing,
      udManagedBy = Nothing,
      udSAMLIdP = Nothing,
      udAccountStatus = Nothing,
      udColourId = Nothing,
      udEmail = Nothing,
      udHandle = Nothing,
      udNormalized = Nothing,
      udName = Nothing,
      udTeam = Nothing,
      udId = uid
    }
