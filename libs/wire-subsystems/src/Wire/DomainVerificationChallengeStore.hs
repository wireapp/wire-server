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
    challengeTokenHash :: Token,
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
      challengeTokenHash = hashToken challenge.token,
      dnsVerificationToken = challenge.dnsVerificationToken
    }

recordInstance ''StoredDomainVerificationChallenge

data DomainVerificationChallengeStore m a where
  Insert :: StoredDomainVerificationChallenge -> DomainVerificationChallengeStore m ()
  Lookup :: ChallengeId -> DomainVerificationChallengeStore m (Maybe StoredDomainVerificationChallenge)
  Delete :: ChallengeId -> DomainVerificationChallengeStore m ()

makeSem ''DomainVerificationChallengeStore
