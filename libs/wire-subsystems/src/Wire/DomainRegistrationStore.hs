{-# LANGUAGE TemplateHaskell #-}

module Wire.DomainRegistrationStore where

import Data.Domain as Domain
import Data.Id
import Data.Misc
import Data.Text as T
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports
import Polysemy
import SAML2.WebSSO qualified as SAML
import Wire.API.EnterpriseLogin

data StoredDomainRegistration = StoredDomainRegistration
  { domain :: Domain,
    domainRedirect :: Maybe DomainRedirectTag,
    teamInvite :: Maybe TeamInviteTag,
    idpId :: Maybe SAML.IdPId,
    backendUrl :: Maybe HttpsUrl,
    team :: Maybe TeamId,
    dnsVerificationToken :: Maybe DnsVerificationToken
  }
  deriving (Show, Eq, Ord, Generic)

recordInstance ''StoredDomainRegistration

data DomainRegistrationStore m a where
  UpsertInternal :: StoredDomainRegistration -> DomainRegistrationStore m ()
  LookupInternal :: Domain -> DomainRegistrationStore m (Maybe StoredDomainRegistration)
  DeleteInternal :: Domain -> DomainRegistrationStore m ()

makeSem ''DomainRegistrationStore

upsert :: (Member DomainRegistrationStore r) => StoredDomainRegistration -> Sem r ()
upsert storedDomainReg = upsertInternal $ storedDomainReg {Wire.DomainRegistrationStore.domain = storedDomainReg.domain & (Domain . T.toLower . domainText)}

lookup :: (Member DomainRegistrationStore r) => Domain -> Sem r (Maybe StoredDomainRegistration)
lookup domain = lookupInternal $ Domain . T.toLower . domainText $ domain

delete :: (Member DomainRegistrationStore r) => Domain -> Sem r ()
delete domain = deleteInternal $ Domain . T.toLower . domainText $ domain

fromStored :: StoredDomainRegistration -> Maybe DomainRegistration
fromStored sdr =
  DomainRegistration sdr.domain
    <$> getDomainRedirect sdr
    <*> getTeamInvite sdr
    <*> pure sdr.dnsVerificationToken
  where
    getTeamInvite :: StoredDomainRegistration -> Maybe TeamInvite
    getTeamInvite = \case
      StoredDomainRegistration _ _ ti _ _ tid _ -> case (ti, tid) of
        (Just AllowedTag, Nothing) -> Just Allowed
        (Just NotAllowedTag, Nothing) -> Just NotAllowed
        (Just TeamTag, Just teamId) -> Just $ Team teamId
        (Nothing, Nothing) -> Just Allowed
        _ -> Nothing

    getDomainRedirect :: StoredDomainRegistration -> Maybe DomainRedirect
    getDomainRedirect = \case
      StoredDomainRegistration _ dr _ ssoId url _ _ -> case (dr, ssoId, url) of
        (Just NoneTag, Nothing, Nothing) -> Just None
        (Just LockedTag, Nothing, Nothing) -> Just Locked
        (Just PreAuthorizedTag, Nothing, Nothing) -> Just PreAuthorized
        (Just SSOTag, Just idpId, Nothing) -> Just $ SSO idpId
        (Just BackendTag, Nothing, Just beUrl) -> Just $ Backend beUrl
        (Just NoRegistrationTag, Nothing, Nothing) -> Just NoRegistration
        (Nothing, Nothing, Nothing) -> Just None
        _ -> Nothing

toStored :: DomainRegistration -> StoredDomainRegistration
toStored dr =
  let (domainRedirect, idpId, backendUrl) = fromDomainRedirect dr.domainRedirect
      (teamInvite, team) = fromTeamInvite dr.teamInvite
   in StoredDomainRegistration dr.domain (Just domainRedirect) (Just teamInvite) idpId backendUrl team dr.dnsVerificationToken
  where
    fromTeamInvite :: TeamInvite -> (TeamInviteTag, Maybe TeamId)
    fromTeamInvite Allowed = (AllowedTag, Nothing)
    fromTeamInvite NotAllowed = (NotAllowedTag, Nothing)
    fromTeamInvite (Team teamId) = (TeamTag, Just teamId)

    fromDomainRedirect :: DomainRedirect -> (DomainRedirectTag, Maybe SAML.IdPId, Maybe HttpsUrl)
    fromDomainRedirect None = (NoneTag, Nothing, Nothing)
    fromDomainRedirect Locked = (LockedTag, Nothing, Nothing)
    fromDomainRedirect (SSO idpId) = (SSOTag, Just idpId, Nothing)
    fromDomainRedirect (Backend url) = (BackendTag, Nothing, Just url)
    fromDomainRedirect NoRegistration = (NoRegistrationTag, Nothing, Nothing)
    fromDomainRedirect PreAuthorized = (PreAuthorizedTag, Nothing, Nothing)
