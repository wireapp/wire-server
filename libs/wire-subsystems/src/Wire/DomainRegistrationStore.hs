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
    dnsVerificationToken :: Maybe DnsVerificationToken,
    authTokenHash :: Maybe Token,
    authorizedTeam :: Maybe TeamId
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
