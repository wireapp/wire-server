module Test.DomainVerification where

import API.Brig
import API.BrigInternal
import API.Common
import Testlib.Prelude

testDomainVerification :: (HasCallStack) => App ()
testDomainVerification = do
  domain <- randomDomain
  bindResponse (domainRegistrationPreAuthorize OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 204
  bindResponse (domainVerificationBackend OwnDomain domain (DomainRegistrationConfigBackend "https://wire.example.com")) $ \resp ->
    resp.status `shouldMatchInt` 200

  bindResponse (getDomainRegistrationFromEmail OwnDomain ("sven@" ++ domain)) \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain-redirect" `shouldMatch` "backend:https://wire.example.com"
