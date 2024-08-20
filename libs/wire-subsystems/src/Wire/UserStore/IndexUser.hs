{-# LANGUAGE RecordWildCards #-}

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
import Database.CQL.Protocol
import Imports
import SAML2.WebSSO qualified as SAML
import URI.ByteString
import Wire.API.User hiding (userId)
import Wire.API.User.Search
import Wire.UserSearch.Types

type Activated = Bool

data WithWritetime a = WithWriteTime {value :: a, writetime :: Writetime a}

data IndexUser = IndexUser
  { userId :: UserId,
    teamId :: Maybe TeamId,
    name :: WithWritetime Name,
    accountStatus :: Maybe (WithWritetime AccountStatus),
    handle :: Maybe (WithWritetime Handle),
    email :: Maybe (WithWritetime EmailAddress),
    colourId :: WithWritetime ColourId,
    activated :: WithWritetime Activated,
    serviceId :: Maybe (WithWritetime ServiceId),
    managedBy :: Maybe (WithWritetime ManagedBy),
    ssoId :: Maybe (WithWritetime UserSSOId),
    unverifiedEmail :: Maybe (WithWritetime EmailAddress)
  }

{- ORMOLU_DISABLE -}
type instance
  TupleType IndexUser =
    ( UserId,
      Maybe TeamId,
      Name, Writetime Name,
      Maybe AccountStatus, Maybe (Writetime AccountStatus),
      Maybe Handle, Maybe (Writetime Handle),
      Maybe Email, Maybe (Writetime Email),
      ColourId, Writetime ColourId,
      Activated, Writetime Activated,
      Maybe ServiceId, Maybe (Writetime ServiceId),
      Maybe ManagedBy, Maybe (Writetime ManagedBy),
      Maybe UserSSOId, Maybe (Writetime UserSSOId),
      Maybe Email, Maybe (Writetime Email)
    )

instance Record IndexUser where
  asTuple (IndexUser {..}) =
    ( userId, teamId,
      name.value, name.writetime,
      value <$> accountStatus, writetime <$> accountStatus,
      value <$> handle, writetime <$> handle,
      value <$> email, writetime <$> email,
      colourId.value, colourId.writetime,
      activated.value, activated.writetime,
      value <$> serviceId, writetime <$> serviceId,
      value <$> managedBy, writetime <$> managedBy,
      value <$> ssoId, writetime <$> ssoId,
      value <$> unverifiedEmail, writetime <$> unverifiedEmail
    )

  asRecord
    ( u, mteam,
      name, tName,
      status, tStatus,
      handle, tHandle,
      email, tEmail,
      colour, tColour,
      activated, tActivated,
      service, tService,
      managedBy, tManagedBy,
      ssoId, tSsoId,
      emailUnvalidated, tEmailUnvalidated
    ) = IndexUser {
          userId = u,
          teamId = mteam,
          name = WithWriteTime name tName,
          accountStatus = WithWriteTime <$> status <*> tStatus,
          handle = WithWriteTime <$> handle <*> tHandle,
          email = WithWriteTime <$> email <*> tEmail,
          colourId = WithWriteTime colour tColour,
          activated = WithWriteTime activated tActivated,
          serviceId = WithWriteTime <$> service <*> tService,
          managedBy = WithWriteTime <$> managedBy <*> tManagedBy,
          ssoId = WithWriteTime <$> ssoId <*> tSsoId,
          unverifiedEmail = WithWriteTime <$> emailUnvalidated <*> tEmailUnvalidated
        }
{- ORMOLU_ENABLE -}

indexUserToVersion :: IndexUser -> IndexVersion
indexUserToVersion IndexUser {..} =
  mkIndexVersion
    [ const () <$$> Just name.writetime,
      const () <$$> fmap writetime accountStatus,
      const () <$$> fmap writetime handle,
      const () <$$> fmap writetime email,
      const () <$$> Just colourId.writetime,
      const () <$$> Just activated.writetime,
      const () <$$> fmap writetime serviceId,
      const () <$$> fmap writetime managedBy,
      const () <$$> fmap writetime ssoId,
      const () <$$> fmap writetime unverifiedEmail
    ]

indexUserToDoc :: SearchVisibilityInbound -> IndexUser -> UserDoc
indexUserToDoc searchVisInbound IndexUser {..} =
  if shouldIndex
    then
      UserDoc
        { udEmailUnvalidated = value <$> unverifiedEmail,
          udSso = sso . value =<< ssoId,
          udScimExternalId = join $ scimExternalId <$> (value <$> managedBy) <*> (value <$> ssoId),
          udSearchVisibilityInbound = Just searchVisInbound,
          udRole = Nothing, -- TODO: This looks weird, why do we have this?
          udCreatedAt = Just . toUTCTimeMillis $ writetimeToUTC activated.writetime,
          udManagedBy = value <$> managedBy,
          udSAMLIdP = idpUrl . value =<< ssoId,
          udAccountStatus = value <$> accountStatus,
          udColourId = Just colourId.value,
          udEmail = value <$> email,
          udHandle = value <$> handle,
          udNormalized = Just $ normalized name.value.fromName,
          udName = Just name.value,
          udTeam = teamId,
          udId = userId
        }
    else -- We insert a tombstone-style user here, as it's easier than
    -- deleting the old one. It's mostly empty, but having the status here
    -- might be useful in the future.
      emptyUserDoc userId
  where
    shouldIndex =
      ( case value <$> accountStatus of
          Nothing -> True
          Just Active -> True
          Just Suspended -> True
          Just Deleted -> False
          Just Ephemeral -> False
          Just PendingInvitation -> False
      )
        && activated.value -- FUTUREWORK: how is this adding to the first case?
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
    { udEmailUnvalidated = Nothing,
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
