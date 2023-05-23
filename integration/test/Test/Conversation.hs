module Test.Conversation where

import qualified API.BrigInternal as Internal
import qualified API.GalleyInternal as API
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testFederationStatus :: HasCallStack => App ()
testFederationStatus = do
  uid <- randomUser OwnDomain def {Internal.team = True}
  federatingRemoteDomain <- asString OtherDomain
  let unknownDomain = "foobar.com"
  let invalidDomain = "c.example.com" -- has no srv record
  bindResponse
    (API.getFederationStatus uid [])
    ( \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "status" `shouldMatch` "fully-connected"
    )

  bindResponse
    (API.getFederationStatus uid [unknownDomain])
    ( \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "discovery-failure"
    )

  bindResponse
    (API.getFederationStatus uid [invalidDomain])
    ( \resp -> do
        resp.status `shouldMatchInt` 422
        resp.json %. "label" `shouldMatch` "invalid-domain"
    )

  bindResponse
    (API.getFederationStatus uid [federatingRemoteDomain])
    ( \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "status" `shouldMatch` "fully-connected"
    )

  bindResponse
    (API.getFederationStatus uid [federatingRemoteDomain, unknownDomain])
    ( \resp -> do
        resp.status `shouldMatchInt` 400
        resp.json %. "label" `shouldMatch` "discovery-failure"
    )
