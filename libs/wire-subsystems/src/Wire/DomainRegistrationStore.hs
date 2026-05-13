{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Wire.DomainRegistrationStore
  ( DomainRegistrationStore (..),
    StoredDomainRegistration (..),
    DomainKey,
    upsert,
    lookup,
    lookupByTeam,
    delete,
    DomainRegistrationRow,
    upsertInternal,
    lookupInternal,
    lookupByTeamInternal,
    deleteInternal,
  )
where

import Cassandra
import Data.ByteString.Conversion
import Data.CaseInsensitive
import Data.CaseInsensitive qualified as CI
import Data.Domain as Domain
import Data.Hashable (hash)
import Data.Id
import Data.Misc
import Data.Text as T
import Data.UUID (UUID)
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports hiding (lookup)
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import SAML2.WebSSO qualified as SAML
import System.Logger.Message qualified as Log
import Wire.API.EnterpriseLogin
import Wire.API.PostgresMarshall
import Wire.MigrationLock

newtype DomainKey = DomainKey {unDomainKey :: CI Text}
  deriving stock (Eq, Ord, Show)

mkDomainKey :: Domain -> DomainKey
mkDomainKey = DomainKey . CI.mk . domainText

unmkDomainKey :: DomainKey -> Domain
unmkDomainKey = Domain . CI.foldedCase . unDomainKey

instance Cql DomainKey where
  ctype = Tagged TextColumn
  toCql = CqlText . CI.foldedCase . unDomainKey
  fromCql (CqlText txt) = pure . DomainKey . CI.mk $ txt
  fromCql _ = Left "DomainKey: Text expected"

instance PostgresMarshall Text DomainKey where
  postgresMarshall = CI.foldedCase . unDomainKey

instance PostgresUnmarshall Text DomainKey where
  postgresUnmarshall = Right . DomainKey . CI.mk

instance MigrationLockable DomainKey where
  lockKey = fromIntegral . hash . CI.foldedCase . unDomainKey
  lockScope = "domain_registration"

type DomainRegistrationRow =
  ( Text,
    Maybe Int32,
    Maybe Int32,
    Maybe UUID,
    Maybe ByteString,
    Maybe UUID,
    Maybe Text,
    Maybe ByteString,
    Maybe UUID,
    Maybe ByteString
  )

instance PostgresMarshall DomainRegistrationRow StoredDomainRegistration where
  postgresMarshall StoredDomainRegistration {..} =
    ( postgresMarshall domain,
      postgresMarshall domainRedirect,
      postgresMarshall teamInvite,
      postgresMarshall idpId,
      postgresMarshall backendUrl,
      postgresMarshall team,
      postgresMarshall dnsVerificationToken,
      postgresMarshall authTokenHash,
      postgresMarshall authorizedTeam,
      postgresMarshall webappUrl
    )

instance PostgresUnmarshall DomainRegistrationRow StoredDomainRegistration where
  postgresUnmarshall
    ( domain,
      domainRedirect,
      teamInvite,
      idpId,
      backendUrl,
      team,
      dnsVerificationToken,
      authTokenHash,
      authorizedTeam,
      webappUrl
      ) =
      StoredDomainRegistration
        <$> postgresUnmarshall domain
        <*> postgresUnmarshall domainRedirect
        <*> postgresUnmarshall teamInvite
        <*> postgresUnmarshall idpId
        <*> postgresUnmarshall backendUrl
        <*> postgresUnmarshall team
        <*> postgresUnmarshall dnsVerificationToken
        <*> postgresUnmarshall authTokenHash
        <*> postgresUnmarshall authorizedTeam
        <*> postgresUnmarshall webappUrl

data StoredDomainRegistration = StoredDomainRegistration
  { domain :: DomainKey,
    domainRedirect :: Maybe DomainRedirectTag,
    teamInvite :: Maybe TeamInviteTag,
    idpId :: Maybe SAML.IdPId,
    backendUrl :: Maybe HttpsUrl,
    team :: Maybe TeamId,
    dnsVerificationToken :: Maybe DnsVerificationToken,
    authTokenHash :: Maybe Token,
    authorizedTeam :: Maybe TeamId,
    webappUrl :: Maybe HttpsUrl
  }
  deriving (Show, Eq, Ord, Generic)

recordInstance ''StoredDomainRegistration

data DomainRegistrationStore m a where
  UpsertInternal :: StoredDomainRegistration -> DomainRegistrationStore m ()
  LookupInternal :: DomainKey -> DomainRegistrationStore m (Maybe StoredDomainRegistration)
  LookupByTeamInternal :: TeamId -> DomainRegistrationStore m [StoredDomainRegistration]
  DeleteInternal :: DomainKey -> DomainRegistrationStore m ()

makeSem ''DomainRegistrationStore

upsert :: (Member DomainRegistrationStore r) => DomainRegistration -> Sem r ()
upsert = send . UpsertInternal . toStored

lookupByTeam :: forall r. (Member DomainRegistrationStore r, Member (Log.Logger (Log.Msg -> Log.Msg)) r) => TeamId -> Sem r [DomainRegistration]
lookupByTeam tid = do
  rows <- send (LookupByTeamInternal tid)
  mRegisteredDomains <- for rows fromStoredWithLogging
  pure $ catMaybes mRegisteredDomains
  where
    fromStoredWithLogging :: StoredDomainRegistration -> Sem r (Maybe DomainRegistration)
    fromStoredWithLogging row = case fromStored row of
      Just dr -> pure (Just dr)
      Nothing -> logInvalidDomainRegistrationError (unmkDomainKey row.domain) $> Nothing

lookup ::
  forall r.
  (Member DomainRegistrationStore r, Member TinyLog r) =>
  Domain ->
  Sem r (Maybe DomainRegistration)
lookup domain =
  (>>= logErrors) . runError @Bool $ do
    sdr <- send (LookupInternal (mkDomainKey domain)) >>= note False
    fromStored sdr & note True
  where
    logErrors :: Either Bool a -> Sem r (Maybe a)
    logErrors (Left False) = pure Nothing
    logErrors (Left True) = logInvalidDomainRegistrationError domain $> Nothing
    logErrors (Right x) = pure (Just x)

logInvalidDomainRegistrationError :: (Member TinyLog r, ToByteString a) => a -> Sem r ()
logInvalidDomainRegistrationError domain =
  Log.err $
    Log.field "domain" (toByteString' domain)
      . Log.msg (Log.val "Invalid stored domain registration")

delete :: (Member DomainRegistrationStore r) => Domain -> Sem r ()
delete = send . DeleteInternal . mkDomainKey

fromStored :: StoredDomainRegistration -> Maybe DomainRegistration
fromStored sdr =
  DomainRegistration (unmkDomainKey sdr.domain) sdr.authorizedTeam
    <$> getDomainRedirect sdr
    <*> getTeamInvite sdr
    <*> pure sdr.dnsVerificationToken
    <*> pure sdr.authTokenHash
  where
    getTeamInvite :: StoredDomainRegistration -> Maybe TeamInvite
    getTeamInvite = \case
      StoredDomainRegistration _ _ ti _ _ tid _ _ _ _ -> case (ti, tid) of
        (Just AllowedTag, Nothing) -> Just Allowed
        (Just NotAllowedTag, Nothing) -> Just NotAllowed
        (Just TeamTag, Just teamId) -> Just $ Team teamId
        (Nothing, Nothing) -> Just Allowed
        _ -> Nothing

    getDomainRedirect :: StoredDomainRegistration -> Maybe DomainRedirect
    getDomainRedirect = \case
      StoredDomainRegistration _ dr _ ssoId url _ _ _ _ webappUrl -> case (dr, ssoId, url, webappUrl) of
        (Just NoneTag, Nothing, Nothing, Nothing) -> Just None
        (Just LockedTag, Nothing, Nothing, Nothing) -> Just Locked
        (Just PreAuthorizedTag, Nothing, Nothing, Nothing) -> Just PreAuthorized
        (Just SSOTag, Just idpId, Nothing, Nothing) -> Just $ SSO idpId
        (Just BackendTag, Nothing, Just beUrl, webAppUrl) -> Just $ Backend beUrl webAppUrl
        (Just NoRegistrationTag, Nothing, Nothing, Nothing) -> Just NoRegistration
        (Nothing, Nothing, Nothing, Nothing) -> Just None
        _ -> Nothing

toStored :: DomainRegistration -> StoredDomainRegistration
toStored dr =
  let (domainRedirect, idpId, backendUrl, webappUrl) = fromDomainRedirect dr.domainRedirect
      (teamInvite, team) = fromTeamInvite dr.teamInvite
   in StoredDomainRegistration
        { domain = mkDomainKey dr.domain,
          domainRedirect = Just domainRedirect,
          teamInvite = Just teamInvite,
          idpId,
          backendUrl,
          team,
          dnsVerificationToken = dr.dnsVerificationToken,
          authTokenHash = dr.authTokenHash,
          authorizedTeam = dr.authorizedTeam,
          webappUrl
        }
  where
    fromTeamInvite :: TeamInvite -> (TeamInviteTag, Maybe TeamId)
    fromTeamInvite Allowed = (AllowedTag, Nothing)
    fromTeamInvite NotAllowed = (NotAllowedTag, Nothing)
    fromTeamInvite (Team teamId) = (TeamTag, Just teamId)

    fromDomainRedirect :: DomainRedirect -> (DomainRedirectTag, Maybe SAML.IdPId, Maybe HttpsUrl, Maybe HttpsUrl)
    fromDomainRedirect None = (NoneTag, Nothing, Nothing, Nothing)
    fromDomainRedirect Locked = (LockedTag, Nothing, Nothing, Nothing)
    fromDomainRedirect (SSO idpId) = (SSOTag, Just idpId, Nothing, Nothing)
    fromDomainRedirect (Backend url webappUrl) = (BackendTag, Nothing, Just url, webappUrl)
    fromDomainRedirect NoRegistration = (NoRegistrationTag, Nothing, Nothing, Nothing)
    fromDomainRedirect PreAuthorized = (PreAuthorizedTag, Nothing, Nothing, Nothing)
