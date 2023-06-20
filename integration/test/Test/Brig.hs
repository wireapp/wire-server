module Test.Brig where

import qualified API.Brig as Public
import qualified API.BrigInternal as Internal
import qualified API.Common as API
import qualified API.GalleyInternal as Internal
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testSearchContactForExternalUsers :: HasCallStack => App ()
testSearchContactForExternalUsers = do
  owner <- randomUser OwnDomain def {Internal.team = True}
  partner <- randomUser OwnDomain def {Internal.team = True}

  bindResponse (Internal.putTeamMember partner (partner %. "team") (API.teamRole "partner")) $ \resp ->
    resp.status `shouldMatchInt` 200

  bindResponse (Public.searchContacts partner (owner %. "name") OwnDomain) $ \resp ->
    resp.status `shouldMatchInt` 403

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
