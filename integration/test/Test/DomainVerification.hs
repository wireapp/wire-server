module Test.DomainVerification where

import API.Brig
import API.BrigInternal
import API.Common
import Testlib.Prelude

testDomainVerification :: (HasCallStack) => App ()
testDomainVerification = do
  domain <- randomDomain

  domainRegistrationPreAuthorize OwnDomain domain >>= assertStatus 204

  (authToken, dnsToken) <-
    bindResponse (domainVerificationToken OwnDomain domain Nothing) $ \resp -> do
      resp.status `shouldMatchInt` 200
      authToken <- resp.json %. "auth_token"
      dnsToken <- resp.json %. "dns_verification_token"
      pure (authToken, dnsToken)

  domainVerificationBackend
    OwnDomain
    domain
    (DomainRegistrationConfigBackend "https://wire.example.com")
    >>= assertStatus 200

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("sven@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain-redirect" `shouldMatch` "backend:https://wire.example.com"
