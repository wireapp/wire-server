{-# LANGUAGE TemplateHaskell #-}

module Wire.DomainVerificationChallengeStore where

import Data.Domain as Domain
import Data.Id
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports
import Polysemy
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Public.Brig.DomainVerification

data StoredDomainVerificationChallenge = StoredDomainVerificationChallenge
  { challengeId :: ChallengeId,
    domain :: Domain,
    challengeToken :: Token,
    dnsVerificationToken :: DnsVerificationToken
  }
  deriving (Show, Eq, Ord, Generic)

mkStoredDomainVerificationChallenge ::
  Domain ->
  DomainVerificationChallenge ->
  StoredDomainVerificationChallenge
mkStoredDomainVerificationChallenge domain challenge =
  StoredDomainVerificationChallenge
    { challengeId = challenge.challengeId,
      domain = domain,
      challengeToken = hashToken challenge.token,
      dnsVerificationToken = challenge.dnsVerificationToken
    }

recordInstance ''StoredDomainVerificationChallenge

data DomainVerificationChallengeStore m a where
  Insert :: StoredDomainVerificationChallenge -> DomainVerificationChallengeStore m ()
  Lookup :: ChallengeId -> DomainVerificationChallengeStore m (Maybe StoredDomainVerificationChallenge)
  Delete :: ChallengeId -> DomainVerificationChallengeStore m ()

makeSem ''DomainVerificationChallengeStore
