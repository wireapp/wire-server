module Test.Brig where

import qualified API.Brig as Public
import qualified API.BrigInternal as Internal
import qualified API.Common as API
import qualified API.GalleyInternal as Internal
import qualified Data.Aeson as Aeson
import Data.String.Conversions
import GHC.Stack
import SetupHelpers
import Testlib.Assertions
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
  let parseFedConns :: HasCallStack => Response -> App [Internal.FedConn]
      parseFedConns resp = fromJust . Aeson.decode . Aeson.encode . fromJust <$> ((`lookupField` "remotes") =<< getJSON 200 resp)

      addOnce :: HasCallStack => Internal.FedConn -> [Internal.FedConn] -> App ()
      addOnce fedConn want = do
        _res <- Internal.createFedConn fedConn
        res <- parseFedConns =<< Internal.readFedConns
        sort res `shouldMatch` sort want

      deleteOnce :: HasCallStack => String -> [Internal.FedConn] -> App ()
      deleteOnce domain want = do
        _res <- Internal.deleteFedConn domain
        res <- parseFedConns =<< Internal.readFedConns
        sort res `shouldMatch` sort want

      deleteFail :: HasCallStack => String -> App ()
      deleteFail del = do
        res <- Internal.deleteFedConn' del
        res.status `shouldMatchInt` 533

      updateOnce :: HasCallStack => String -> Internal.FedConn -> [Internal.FedConn] -> App ()
      updateOnce domain fedConn want = do
        _res <- Internal.updateFedConn domain fedConn
        res <- parseFedConns =<< Internal.readFedConns
        sort res `shouldMatch` sort want

      updateFail :: HasCallStack => String -> Internal.FedConn -> App ()
      updateFail domain fedConn = do
        res <- Internal.updateFedConn' domain fedConn
        res.status `shouldMatchInt` 533

  let remote1, remote1', remote1'' :: Internal.FedConn
      remote1 = Internal.FedConn (cs "good.example.com") "no_search"
      remote1' = remote1 {Internal.searchStrategy = "full_search"}
      remote1'' = remote1 {Internal.domain = "meh.example.com"}

      cfgRemotesExpect :: Internal.FedConn
      cfgRemotesExpect = Internal.FedConn (cs "example.com") "full_search"

  resetFedConns
  cfgRemotes <- parseFedConns =<< Internal.readFedConns
  cfgRemotes `shouldMatch` [cfgRemotesExpect]

  -- entries present in the config file can be idempotently added, but cannot be deleted or
  -- updated.
  addOnce cfgRemotesExpect [cfgRemotesExpect]
  deleteFail (Internal.domain cfgRemotesExpect)
  updateFail (Internal.domain cfgRemotesExpect) cfgRemotesExpect {Internal.searchStrategy = "no_search"}

  -- create
  addOnce remote1 (remote1 : cfgRemotes)
  addOnce remote1 (remote1 : cfgRemotes) -- idempotency

  -- update
  updateOnce (Internal.domain remote1) remote1' (remote1' : cfgRemotes)
  updateFail (Internal.domain remote1) remote1''

  -- delete
  deleteOnce (Internal.domain remote1) cfgRemotes
  deleteOnce (Internal.domain remote1) cfgRemotes -- idempotency
