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

-- Parse a given JSON value to a structure.
-- Calls `assertFailure` if the value cannot
-- be parsed to the desired type.
parseJSONApp :: FromJSON a => Value -> App a
parseJSONApp = either assertFailure pure . parseEither parseJSON

testCrudFederationRemotes :: HasCallStack => App ()
testCrudFederationRemotes = do
  let parseFedConns :: HasCallStack => Response -> App [Internal.FedConn]
      parseFedConns resp = getJSON 200 resp %. "remotes" & asList >>= mapM parseJSONApp
      addOnce :: HasCallStack => Internal.FedConn -> [Internal.FedConn] -> App ()
      addOnce fedConn want = do
        res <- Internal.createFedConn OwnDomain fedConn
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 200
        res2 <- parseFedConns =<< Internal.readFedConns OwnDomain
        sort res2 `shouldMatch` sort want

      addFail :: HasCallStack => MakesValue fedConn => fedConn -> App ()
      addFail fedConn = do
        res <- Internal.createFedConn' OwnDomain fedConn
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 533

      deleteOnce :: String -> [Internal.FedConn] -> App ()
      deleteOnce domain want = do
        res <- Internal.deleteFedConn OwnDomain domain
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 200
        res2 <- parseFedConns =<< Internal.readFedConns OwnDomain
        sort res2 `shouldMatch` sort want

      deleteFail :: HasCallStack => String -> App ()
      deleteFail del = do
        res <- Internal.deleteFedConn' OwnDomain del
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 533

      updateOnce :: HasCallStack => String -> Internal.FedConn -> [Internal.FedConn] -> App ()
      updateOnce domain fedConn want = do
        res <- Internal.updateFedConn OwnDomain domain fedConn
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 200
        res2 <- parseFedConns =<< Internal.readFedConns OwnDomain
        sort res2 `shouldMatch` sort want

      updateFail :: (MakesValue fedConn, HasCallStack) => String -> fedConn -> App ()
      updateFail domain fedConn = do
        res <- Internal.updateFedConn' OwnDomain domain fedConn
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 533

  let remote1, remote1', remote1'' :: Internal.FedConn
      remote1 = Internal.FedConn (cs "good.example.com") "no_search"
      remote1' = remote1 {Internal.searchStrategy = "full_search"}
      remote1'' = remote1 {Internal.domain = "meh.example.com"}

      cfgRemotesExpect :: Internal.FedConn
      cfgRemotesExpect = Internal.FedConn (cs "example.com") "full_search"

  resetFedConns OwnDomain
  cfgRemotes <- parseFedConns =<< Internal.readFedConns OwnDomain
  cfgRemotes `shouldMatch` [cfgRemotesExpect]
  -- entries present in the config file can be idempotently added if identical, but cannot be
  -- updated, deleted or updated.
  addOnce cfgRemotesExpect [cfgRemotesExpect]
  addFail (cfgRemotesExpect {Internal.searchStrategy = "no_search"})
  deleteFail (Internal.domain cfgRemotesExpect)
  updateFail (Internal.domain cfgRemotesExpect) (cfgRemotesExpect {Internal.searchStrategy = "no_search"})
  -- create
  addOnce remote1 (remote1 : cfgRemotes)
  addOnce remote1 (remote1 : cfgRemotes) -- idempotency
  -- update
  updateOnce (Internal.domain remote1) remote1' (remote1' : cfgRemotes)
  updateFail (Internal.domain remote1) remote1''
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
