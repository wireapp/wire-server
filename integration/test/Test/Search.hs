module Test.Search where

import API.Brig qualified as BrigP
import API.BrigInternal qualified as BrigI
import API.Common qualified as API
import API.GalleyInternal qualified as GalleyI
import GHC.Stack
import SetupHelpers
import Testlib.Assertions
import Testlib.Prelude

--------------------------------------------------------------------------------
-- LOCAL SEARCH

testSearchContactForExternalUsers :: HasCallStack => App ()
testSearchContactForExternalUsers = do
  owner <- randomUser OwnDomain def {BrigI.team = True}
  partner <- randomUser OwnDomain def {BrigI.team = True}

  bindResponse (GalleyI.putTeamMember partner (partner %. "team") (API.teamRole "partner")) $ \resp ->
    resp.status `shouldMatchInt` 200

  bindResponse (BrigP.searchContacts partner (owner %. "name") OwnDomain) $ \resp ->
    resp.status `shouldMatchInt` 403

--------------------------------------------------------------------------------
-- FEDERATION SEARCH

testRemoteUserSearch :: HasCallStack => App ()
testRemoteUserSearch = do
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 "full_search" "allow_all")
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 "full_search" "allow_all")

    u1 <- randomUser d1 def
    u2 <- randomUser d2 def
    BrigI.refreshIndex d2
    uidD2 <- objId u2

    bindResponse (BrigP.searchContacts u1 (u2 %. "name") d2) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> assertFailure "Expected a non empty result, but got an empty one"
        doc : _ -> doc %. "id" `shouldMatch` uidD2

testRemoteUserSearchExactHandle :: HasCallStack => App ()
testRemoteUserSearchExactHandle = do
  startDynamicBackends [def, def] $ \[d1, d2] -> do
    void $ BrigI.createFedConn d2 (BrigI.FedConn d1 "exact_handle_search" "allow_all")
    void $ BrigI.createFedConn d1 (BrigI.FedConn d2 "exact_handle_search" "allow_all")

    u1 <- randomUser d1 def
    u2 <- randomUser d2 def
    u2Handle <- API.randomHandle
    bindResponse (BrigP.putHandle u2 u2Handle) $ assertSuccess
    BrigI.refreshIndex d2

    bindResponse (BrigP.searchContacts u1 u2Handle d2) $ \resp -> do
      resp.status `shouldMatchInt` 200
      docs <- resp.json %. "documents" >>= asList
      case docs of
        [] -> assertFailure "Expected a non empty result, but got an empty one"
        doc : _ -> objQid doc `shouldMatch` objQid u2
