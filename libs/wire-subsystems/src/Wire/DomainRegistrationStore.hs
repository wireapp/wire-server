{-# LANGUAGE TemplateHaskell #-}

module Wire.DomainRegistrationStore where

import Data.Domain
import Data.Id
import Data.Misc
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports
import Polysemy
import SAML2.WebSSO qualified as SAML
import Wire.API.EnterpriseLogin

data StoredDomainRegistration = StoredDomainRegistration
  { domain :: Domain,
    domainRedirect :: DomainRedirectTag,
    teamInvite :: TeamInviteTag,
    idpId :: Maybe SAML.IdPId,
    backendUrl :: Maybe HttpsUrl,
    team :: Maybe TeamId,
    dnsVerificationCode :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

recordInstance ''StoredDomainRegistration

data DomainRegistrationStore m a where
  Upsert :: StoredDomainRegistration -> DomainRegistrationStore m ()
  Lookup :: Domain -> DomainRegistrationStore m (Maybe StoredDomainRegistration)
  Delete :: Domain -> DomainRegistrationStore m ()

makeSem ''DomainRegistrationStore
