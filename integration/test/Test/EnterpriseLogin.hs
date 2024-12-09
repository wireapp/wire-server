module Test.EnterpriseLogin where

import API.BrigInternal
import API.Common
import Testlib.Prelude

testDomainRegistrationLock :: App ()
testDomainRegistrationLock = do
  domain <- randomDomain
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 404
  bindResponse (domainRegistrationLock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain" `shouldMatch` domain
    resp.json %. "domain_redirect" `shouldMatch` "locked"
    resp.json %. "team_invite" `shouldMatch` "allowed"
