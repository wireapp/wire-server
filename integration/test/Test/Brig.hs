module Test.Brig where

import qualified API.Brig as Public
import qualified API.BrigInternal as Internal
import qualified API.Common as API
import qualified API.GalleyInternal as Internal
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testSearchContactForExternalUsers :: HasCallStack => App ()
testSearchContactForExternalUsers = do
  owner <- randomUser ownDomain def {Internal.team = True}
  partner <- randomUser ownDomain def {Internal.team = True}

  bindResponse (Internal.putTeamMember partner (partner %. "team") (API.teamRole "partner")) $ \resp ->
    resp.status `shouldMatchInt` 200

  bindResponse (Public.searchContacts partner (owner %. "name")) $ \resp ->
    resp.status `shouldMatchInt` 403

testCrudFederationRemotes :: HasCallStack => App ()
testCrudFederationRemotes = do
  -- Delete the remotes from the database
  -- This doesn't do anything with the remotes
  -- defined in config files.
  resetFederationRemotes opts brig

  res1 <- getFederationRemotes brig
  liftIO $ assertEqual "should return config values" cfgRemotes res1

  let remote1 = FederationDomainConfig (Domain "good.example.com") NoSearch
  addFederationRemote brig remote1
  res2 <- getFederationRemotes brig
  liftIO $ assertEqual "should return config values and good.example.com" (sort $ remote1 : cfgRemotes) (sort res2)

  -- idempotency
  addFederationRemote brig remote1
  res2' <- getFederationRemotes brig
  liftIO $ assertEqual "should return config values and good.example.com" (sort $ remote1 : cfgRemotes) (sort res2')

  let remote2 = FederationDomainConfig (Domain "evil.example.com") ExactHandleSearch
  addFederationRemote brig remote2
  res3 <- getFederationRemotes brig
  liftIO $ assertEqual "should return config values and {good,evil}.example.com" (nub $ sort $ cfgRemotes <> [remote1, remote2]) (sort res3)

  deleteFederationRemote brig (domain remote1)
  res4 <- getFederationRemotes brig
  liftIO $ assertEqual "should return config values and evil.example.com" (nub $ sort $ cfgRemotes <> [remote2]) (sort res4)

  -- deleting from the config file triggers an error
  deleteFederationRemote' id brig (domain $ head $ cfgRemotes) !!! const 533 === statusCode

  -- updating search strategy works
  let remote2' = remote2 {cfgSearchPolicy = NoSearch}
  updateFederationRemote brig (domain remote2) remote2'
  res5 <- getFederationRemotes brig
  -- (move the dynamic remotes to the beginning here to make sure we look for `remote2'`, not `remote`.)
  liftIO $ assertEqual "should be NoSearch" (nub $ sort $ [remote1, remote2'] <> cfgRemotes) (sort res5)

  -- updating from config file fails
  updateFederationRemote' id brig (domain $ head $ cfgRemotes) (head $ cfgRemotes) !!! const 533 === statusCode

  -- updating domain fails
  let remote2'' = remote2' {domain = Domain "broken.example.com"}
  updateFederationRemote' id brig (domain remote2) remote2'' !!! const 533 === statusCode

  -- TODO: how do we test that the TVar is updated in all services?  some fancy unit test?
  -- duplicate internal end-point to all services, and implement the hanlers in a library?
  pure ()
  where
    cfgRemotes = fromMaybe [] . Opt.setFederationDomainConfigs $ Opt.optSettings opts
