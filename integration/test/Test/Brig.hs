module Test.Brig where

import qualified API.Brig as Public
import qualified API.BrigInternal as Internal
import qualified API.Common as API
import qualified API.GalleyInternal as Internal
import Data.Aeson.Types
import qualified Data.Set as Set
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

  bindResponse (Public.searchContacts partner (owner %. "name") OwnDomain) $ \resp ->
    resp.status `shouldMatchInt` 403

testCrudFederationRemotes :: HasCallStack => App ()
testCrudFederationRemotes = do
  let parseFedConns :: HasCallStack => Response -> App [Value]
      parseFedConns resp =
        -- Pick out the list of federation domain configs
        getJSON 200 resp %. "remotes"
          & asList
          -- Enforce that the values are objects and not something else
          >>= traverse (fmap Object . asObject)

      addOnce :: (MakesValue fedConn, Ord fedConn2, ToJSON fedConn2, MakesValue fedConn2, HasCallStack) => fedConn -> [fedConn2] -> App ()
      addOnce fedConn want = do
        res <- Internal.createFedConn OwnDomain fedConn
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 200
        res2 <- parseFedConns =<< Internal.readFedConns OwnDomain
        sort res2 `shouldMatch` sort want

      addFail :: HasCallStack => MakesValue fedConn => fedConn -> App ()
      addFail fedConn = do
        res <- Internal.createFedConn' OwnDomain fedConn
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 533

      deleteOnce :: (Ord fedConn, ToJSON fedConn, MakesValue fedConn) => String -> [fedConn] -> App ()
      deleteOnce domain want = do
        res <- Internal.deleteFedConn OwnDomain domain
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 200
        res2 <- parseFedConns =<< Internal.readFedConns OwnDomain
        sort res2 `shouldMatch` sort want

      deleteFail :: HasCallStack => String -> App ()
      deleteFail del = do
        res <- Internal.deleteFedConn' OwnDomain del
        addFailureContext ("res = " <> show res) $ res.status `shouldMatchInt` 533

      updateOnce :: (MakesValue fedConn, Ord fedConn2, ToJSON fedConn2, MakesValue fedConn2, HasCallStack) => String -> fedConn -> [fedConn2] -> App ()
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

  remote1J <- make remote1
  remote1J' <- make remote1'

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
  addOnce remote1 $ (remote1J : cfgRemotes)
  addOnce remote1 $ (remote1J : cfgRemotes) -- idempotency
  -- update
  updateOnce (Internal.domain remote1) remote1' (remote1J' : cfgRemotes)
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

-- | See https://docs.wire.com/understand/api-client-perspective/swagger.html
testSwagger :: HasCallStack => App ()
testSwagger = do
  let existingVersions :: [Int]
      existingVersions = [0, 1, 2, 3, 4]

      internalApis :: [String]
      internalApis = ["brig", "cannon", "cargohold", "cannon", "spar"]

  bindResponse Public.getApiVersions $ \resp -> do
    resp.status `shouldMatchInt` 200
    actualVersions :: [Int] <- do
      sup <- resp.json %. "supported" & asListOf asInt
      dev <- resp.json %. "development" & asListOf asInt
      pure $ sup <> dev
    assertBool ("unexpected actually existing versions: " <> show actualVersions) $
      -- make sure nobody has added a new version without adding it to `existingVersions`.
      -- ("subset" because blocked versions like v3 are not actually existing, but still
      -- documented.)
      Set.fromList actualVersions `Set.isSubsetOf` Set.fromList existingVersions

  bindResponse Public.getSwaggerPublicTOC $ \resp -> do
    resp.status `shouldMatchInt` 200
    cs resp.body `shouldContainString` "<html>"

  forM_ existingVersions $ \v -> do
    bindResponse (Public.getSwaggerPublicAllUI v) $ \resp -> do
      resp.status `shouldMatchInt` 200
      cs resp.body `shouldContainString` "<!DOCTYPE html>"
    bindResponse (Public.getSwaggerPublicAllJson v) $ \resp -> do
      resp.status `shouldMatchInt` 200
      void resp.json

  -- FUTUREWORK: Implement Public.getSwaggerInternalTOC (including the end-point); make sure
  -- newly added internal APIs make this test fail if not added to `internalApis`.

  forM_ internalApis $ \api -> do
    bindResponse (Public.getSwaggerInternalUI api) $ \resp -> do
      resp.status `shouldMatchInt` 200
      cs resp.body `shouldContainString` "<!DOCTYPE html>"
    bindResponse (Public.getSwaggerInternalJson api) $ \resp -> do
      resp.status `shouldMatchInt` 200
      void resp.json
