{-# LANGUAGE TemplateHaskell #-}

module Wire.DomainVerificationChallengeStore where

import Data.Domain as Domain
import Data.Id
import Database.CQL.Protocol (Record (..), TupleType, recordInstance)
import Imports
import Polysemy
import Wire.API.EnterpriseLogin

data StoredDomainVerificationChallenge = StoredDomainVerificationChallenge
  { challengeId :: ChallengeId,
    domain :: Domain,
    challengeToken :: Token,
    dnsVerificationToken :: DnsVerificationToken
  }
  deriving (Show, Eq, Ord, Generic)

recordInstance ''StoredDomainVerificationChallenge

data DomainVerificationChallengeStore m a where
  Insert :: StoredDomainVerificationChallenge -> DomainVerificationChallengeStore m ()
  Lookup :: ChallengeId -> DomainVerificationChallengeStore m (Maybe StoredDomainVerificationChallenge)
  Delete :: ChallengeId -> DomainVerificationChallengeStore m ()

makeSem ''DomainVerificationChallengeStore
