module Test.Conversation where

import qualified API.BrigInternal as Internal
import qualified API.Galley as API
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testFederationStatus :: HasCallStack => App ()
testFederationStatus = do
  uid <- randomUser ownDomain def {Internal.team = True}
  bindResponse
    (API.getFederationStatus uid [])
    ( \resp -> do
        resp.status `shouldMatchInt` 200
        resp %. "status" `shouldMatch` "fully-connected"
    )

  bindResponse
    (API.getFederationStatus uid ["foobar.com"])
    ( \resp -> do
        resp.status `shouldMatchInt` 400
        resp %. "label" `shouldMatch` "discovery-failure"
    )

  d <- otherDomain
  bindResponse
    (API.getFederationStatus uid [d])
    ( \resp -> do
        resp.status `shouldMatchInt` 200
        resp %. "status" `shouldMatch` "fully-connected"
    )

  bindResponse
    (API.getFederationStatus uid [d, "c.example.com"])
    ( \resp -> do
        resp.status `shouldMatchInt` 200
        resp %. "status" `shouldMatch` "non-fully-connected"
    )
