module Test.Brig where

import qualified API.Brig as Public
import qualified API.BrigInternal as Internal
import qualified API.Common as API
import qualified API.GalleyInternal as Internal
import Data.Aeson.Types
import Data.String.Conversions
import GHC.Stack
import SetupHelpers
import Testlib.Assertions
import Testlib.Prelude

testSearchContactForExternalUsers :: HasCallStack => App ()
testSearchContactForExternalUsers = do
  owner <- randomUser OwnDomain def {Internal.team = True}
  partner <- randomUser OwnDomain def {Internal.team = True}

  bindResponse (Internal.putTeamMember partner (partner %. "team") (API.teamRole "partner")) $ \resp ->
    resp.status `shouldMatchInt` 200

  bindResponse (Public.searchContacts partner (owner %. "name")) $ \resp ->
    resp.status `shouldMatchInt` 403

testCrudFederationRemotes :: HasCallStack => App ()
testCrudFederationRemotes = do
  let parseFedConns :: HasCallStack => Response -> App [Value]
      parseFedConns resp =
        -- Pick out the list of federation domain configs
        getJSON 200 resp %. "remotes" & asList
        -- Enforce that the values are objects and not something else
        >>= traverse (fmap Object . asObject)
      addOnce :: (MakesValue fedConn, HasCallStack) => fedConn -> [fedConn] -> App ()
      addOnce fedConn want = do
        res <- Internal.createFedConn OwnDomain fedConn
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 200
        res2 <- parseFedConns =<< Internal.readFedConns OwnDomain
        want' <- traverse make want
        sort res2 `shouldMatch` sort want'

      addFail :: HasCallStack => MakesValue fedConn => fedConn -> App ()
      addFail fedConn = do
        res <- Internal.createFedConn' OwnDomain fedConn
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 533

      deleteOnce :: MakesValue fedConn => String -> [fedConn] -> App ()
      deleteOnce domain want = do
        res <- Internal.deleteFedConn OwnDomain domain
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 200
        res2 <- parseFedConns =<< Internal.readFedConns OwnDomain
        want' <- traverse make want
        sort res2 `shouldMatch` sort want'

      deleteFail :: HasCallStack => String -> App ()
      deleteFail del = do
        res <- Internal.deleteFedConn' OwnDomain del
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 533

      updateOnce :: (MakesValue fedConn, HasCallStack) => String -> fedConn -> [fedConn] -> App ()
      updateOnce domain fedConn want = do
        res <- Internal.updateFedConn OwnDomain domain fedConn
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 200
        res2 <- parseFedConns =<< Internal.readFedConns OwnDomain
        want' <- traverse make want
        sort res2 `shouldMatch` sort want'

      updateFail :: (MakesValue fedConn, HasCallStack) => String -> fedConn -> App ()
      updateFail domain fedConn = do
        res <- Internal.updateFedConn' OwnDomain domain fedConn
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 533

  let remote1, remote1', remote1'' :: Internal.FedConn
      remote1J, remote1J', remote1J'' :: Value
      remote1 = Internal.FedConn (cs "good.example.com") "no_search"
      remote1' = remote1 {Internal.searchStrategy = "full_search"}
      remote1'' = remote1 {Internal.domain = "meh.example.com"}
      remote1J = toJSON remote1
      remote1J' = toJSON remote1'
      remote1J'' = toJSON remote1''

      cfgRemotesExpect :: Internal.FedConn
      cfgRemotesExpect = Internal.FedConn (cs "example.com") "full_search"

  resetFedConns OwnDomain
  cfgRemotes <- parseFedConns =<< Internal.readFedConns OwnDomain
  cfgRemotes `shouldMatch` [toJSON cfgRemotesExpect]
  -- entries present in the config file can be idempotently added if identical, but cannot be
  -- updated, deleted or updated.
  addOnce cfgRemotesExpect [cfgRemotesExpect]
  addFail (cfgRemotesExpect {Internal.searchStrategy = "no_search"})
  deleteFail (Internal.domain cfgRemotesExpect)
  updateFail (Internal.domain cfgRemotesExpect) (cfgRemotesExpect {Internal.searchStrategy = "no_search"})
  -- create
  addOnce remote1J $ (remote1J : cfgRemotes)
  addOnce remote1J $ (remote1J : cfgRemotes) -- idempotency
  -- update
  updateOnce (Internal.domain remote1) remote1J' (remote1J' : cfgRemotes)
  updateFail (Internal.domain remote1) remote1J''
  -- delete
  deleteOnce (Internal.domain remote1) cfgRemotes
  deleteOnce (Internal.domain remote1) cfgRemotes -- idempotency

testCrudOAuthClient :: HasCallStack => App ()
testCrudOAuthClient = do
  user <- randomUser OwnDomain def
  let appName = "foobar"
  let url = "https://example.com/callback.html"
  clientId <- bindResponse (Internal.registerOAuthClient user appName url) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "client_id"
  bindResponse (Internal.getOAuthClient user clientId) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "application_name" `shouldMatch` appName
    resp.json %. "redirect_url" `shouldMatch` url
  let newName = "barfoo"
  let newUrl = "https://example.com/callback2.html"
  bindResponse (Internal.updateOAuthClient user clientId newName newUrl) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "application_name" `shouldMatch` newName
    resp.json %. "redirect_url" `shouldMatch` newUrl
  bindResponse (Internal.deleteOAuthClient user clientId) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (Internal.getOAuthClient user clientId) $ \resp -> do
    resp.status `shouldMatchInt` 404
