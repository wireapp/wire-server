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

module Test.Wire.API.Golden.Manual.EnterpriseLogin where

import Data.Domain (Domain (Domain))
import Data.Id
import Data.Misc (HttpsUrl (HttpsUrl))
import Data.UUID qualified as UUID
import Imports
import SAML2.WebSSO qualified as SAML
import URI.ByteString (parseURI, strictURIParserOptions)
import Wire.API.EnterpriseLogin

testObject_DomainRegistration_1 :: DomainRegistration
testObject_DomainRegistration_1 =
  DomainRegistration
    { domain = Domain "example.com",
      domainRedirect = Locked,
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistration_2 :: DomainRegistration
testObject_DomainRegistration_2 =
  DomainRegistration
    { domain = Domain "example.com",
      domainRedirect = None,
      teamInvite = NotAllowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistration_3 :: DomainRegistration
testObject_DomainRegistration_3 =
  DomainRegistration
    { domain = Domain "example.com",
      domainRedirect = SSO (SAML.IdPId $ fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284")),
      teamInvite = Team $ Id (fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284")),
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistration_4 :: DomainRegistration
testObject_DomainRegistration_4 =
  DomainRegistration
    { domain = Domain "example.com",
      domainRedirect = Backend (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://example.com/inv14"))),
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistration_5 :: DomainRegistration
testObject_DomainRegistration_5 =
  DomainRegistration
    { domain = Domain "example.com",
      domainRedirect = NoRegistration,
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistration_6 :: DomainRegistration
testObject_DomainRegistration_6 =
  DomainRegistration
    { domain = Domain "example.com",
      domainRedirect = PreAuthorized,
      teamInvite = Allowed,
      dnsVerificationToken = Just $ DnsVerificationToken "wire-domain-Ym9vCg::example.com"
    }

testObject_DomainRegistrationUpdate_1 :: DomainRegistrationUpdate
testObject_DomainRegistrationUpdate_1 =
  DomainRegistrationUpdate
    { domainRedirect = Locked,
      teamInvite = Allowed
    }

testObject_DomainRegistrationUpdate_2 :: DomainRegistrationUpdate
testObject_DomainRegistrationUpdate_2 =
  DomainRegistrationUpdate
    { domainRedirect = None,
      teamInvite = NotAllowed
    }

testObject_DomainRegistrationUpdate_3 :: DomainRegistrationUpdate
testObject_DomainRegistrationUpdate_3 =
  DomainRegistrationUpdate
    { domainRedirect = SSO (SAML.IdPId $ fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284")),
      teamInvite = Allowed
    }

testObject_DomainRegistrationUpdate_4 :: DomainRegistrationUpdate
testObject_DomainRegistrationUpdate_4 =
  DomainRegistrationUpdate
    { domainRedirect = Backend (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://example.com/inv14"))),
      teamInvite = Allowed
    }

testObject_DomainRegistrationUpdate_5 :: DomainRegistrationUpdate
testObject_DomainRegistrationUpdate_5 =
  DomainRegistrationUpdate
    { domainRedirect = PreAuthorized,
      teamInvite = Allowed
    }

testObject_DomainRegistrationUpdate_6 :: DomainRegistrationUpdate
testObject_DomainRegistrationUpdate_6 =
  DomainRegistrationUpdate
    { domainRedirect = NoRegistration,
      teamInvite = Team $ Id (fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284"))
    }
