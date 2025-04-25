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

testObject_DomainRegistrationResponseV8_1 :: DomainRegistrationResponseV8
testObject_DomainRegistrationResponseV8_1 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect = Locked,
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV8_2 :: DomainRegistrationResponseV8
testObject_DomainRegistrationResponseV8_2 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect = None,
      teamInvite = NotAllowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV8_3 :: DomainRegistrationResponseV8
testObject_DomainRegistrationResponseV8_3 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect = SSO (SAML.IdPId $ fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284")),
      teamInvite = Team $ Id (fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284")),
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV8_4 :: DomainRegistrationResponseV8
testObject_DomainRegistrationResponseV8_4 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect = Backend (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://example.com/inv14"))) Nothing,
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV8_5 :: DomainRegistrationResponseV8
testObject_DomainRegistrationResponseV8_5 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Id <$> UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284",
      domainRedirect = NoRegistration,
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV8_6 :: DomainRegistrationResponseV8
testObject_DomainRegistrationResponseV8_6 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect = PreAuthorized,
      teamInvite = Allowed,
      dnsVerificationToken = Just $ DnsVerificationToken "jfdjsejsdjsdfjsdfjlwejwekljwef"
    }

testObject_DomainRegistrationResponseV8_7 :: DomainRegistrationResponseV8
testObject_DomainRegistrationResponseV8_7 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect =
        Backend
          (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://example.com/inv14")))
          (Just (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://webapp.example.com")))),
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV9_1 :: DomainRegistrationResponseV9
testObject_DomainRegistrationResponseV9_1 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect = Locked,
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV9_2 :: DomainRegistrationResponseV9
testObject_DomainRegistrationResponseV9_2 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect = None,
      teamInvite = NotAllowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV9_3 :: DomainRegistrationResponseV9
testObject_DomainRegistrationResponseV9_3 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect = SSO (SAML.IdPId $ fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284")),
      teamInvite = Team $ Id (fromJust (UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284")),
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV9_4 :: DomainRegistrationResponseV9
testObject_DomainRegistrationResponseV9_4 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect = Backend (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://example.com/inv14"))) Nothing,
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV9_5 :: DomainRegistrationResponseV9
testObject_DomainRegistrationResponseV9_5 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Id <$> UUID.fromString "abf7c0b2-f4e6-4588-8fbb-3b4bf2344284",
      domainRedirect = NoRegistration,
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
    }

testObject_DomainRegistrationResponseV9_6 :: DomainRegistrationResponseV9
testObject_DomainRegistrationResponseV9_6 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect = PreAuthorized,
      teamInvite = Allowed,
      dnsVerificationToken = Just $ DnsVerificationToken "jfdjsejsdjsdfjsdfjlwejwekljwef"
    }

testObject_DomainRegistrationResponseV9_7 :: DomainRegistrationResponseV9
testObject_DomainRegistrationResponseV9_7 =
  DomainRegistrationResponse
    { domain = Domain "example.com",
      authorizedTeam = Nothing,
      domainRedirect =
        Backend
          (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://example.com/inv14")))
          (Just (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://webapp.example.com")))),
      teamInvite = Allowed,
      dnsVerificationToken = Nothing
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

-- TODO: Add test with webapp Url (kind of 4b)
testObject_DomainRegistrationUpdate_4 :: DomainRegistrationUpdate
testObject_DomainRegistrationUpdate_4 =
  DomainRegistrationUpdate
    { domainRedirect = Backend (HttpsUrl (fromRight' (parseURI strictURIParserOptions "https://example.com/inv14"))) Nothing,
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
