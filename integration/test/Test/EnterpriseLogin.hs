module Test.EnterpriseLogin where

import API.BrigInternal
import Testlib.Prelude

testDomainRegistrationLock :: App ()
testDomainRegistrationLock = do
  let domain = "example.com"
  bindResponse (domainRegistrationLock OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (getDomainRegistration OwnDomain domain) $ \resp -> do
    resp.status `shouldMatchInt` 200
