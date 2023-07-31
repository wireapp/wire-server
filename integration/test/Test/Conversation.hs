module Test.Conversation where

import qualified API.BrigInternal as Internal
import API.Galley (defProteus, postConversation, qualifiedUsers)
import qualified API.GalleyInternal as API
import Control.Applicative
import qualified Data.Aeson as Aeson
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testDynamicBackendsFullyConnectedWhenAllowAll :: HasCallStack => App ()
testDynamicBackendsFullyConnectedWhenAllowAll = do
  let overrides =
        def {dbBrig = setField "optSettings.setFederationStrategy" "allowAll"}
          <> fullSearchWithAll
  startDynamicBackends [overrides, overrides, overrides] $ \dynDomains -> do
    [domainA, domainB, domainC] <- pure dynDomains
    uidA <- randomUser domainA def {Internal.team = True}
    uidB <- randomUser domainA def {Internal.team = True}
    uidC <- randomUser domainA def {Internal.team = True}
    let assertConnected u d d' =
          bindResponse
            (API.getFederationStatus u [d, d'])
            $ \resp -> do
              resp.status `shouldMatchInt` 200
              resp.json %. "status" `shouldMatch` "fully-connected"
    assertConnected uidA domainB domainC
    assertConnected uidB domainA domainC
    assertConnected uidC domainA domainB

testDynamicBackendsNotFederating :: HasCallStack => App ()
testDynamicBackendsNotFederating = do
  let overrides =
        def
          { dbBrig =
              setField "optSettings.setFederationStrategy" "allowNone"
          }
  startDynamicBackends [overrides, overrides, overrides] $
    \dynDomains -> do
      [domainA, domainB, domainC] <- pure dynDomains
      uidA <- randomUser domainA def {Internal.team = True}
      unrace
        $ bindResponse
          (API.getFederationStatus uidA [domainB, domainC])
        $ \resp -> do
          resp.status `shouldMatchInt` 422
          resp.json %. "label" `shouldMatch` "federation-denied"

testDynamicBackendsFullyConnectedWhenAllowDynamic :: HasCallStack => App ()
testDynamicBackendsFullyConnectedWhenAllowDynamic = do
  let overrides =
        setField "optSettings.setFederationStrategy" "allowDynamic"
          >=> removeField "optSettings.setFederationDomainConfigs"
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {dbBrig = overrides},
      def {dbBrig = overrides},
      def {dbBrig = overrides}
    ]
    $ \dynDomains -> do
      domains@[domainA, domainB, domainC] <- pure dynDomains
      sequence_ [Internal.createFedConn x (Internal.FedConn y "full_search") | x <- domains, y <- domains, x /= y]
      uidA <- randomUser domainA def {Internal.team = True}
      uidB <- randomUser domainB def {Internal.team = True}
      uidC <- randomUser domainC def {Internal.team = True}
      let assertConnected u d d' =
            bindResponse
              (API.getFederationStatus u [d, d'])
              $ \resp -> do
                resp.status `shouldMatchInt` 200
                resp.json %. "status" `shouldMatch` "fully-connected"
      unrace $ assertConnected uidA domainB domainC
      unrace $ assertConnected uidB domainA domainC
      unrace $ assertConnected uidC domainA domainB

testDynamicBackendsNotFullyConnected :: HasCallStack => App ()
testDynamicBackendsNotFullyConnected = do
  let overrides =
        def
          { dbBrig =
              setField "optSettings.setFederationStrategy" "allowDynamic"
                >=> removeField "optSettings.setFederationDomainConfigs"
                >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
          }
  startDynamicBackends [overrides, overrides, overrides] $
    \dynDomains -> do
      domains@[domainA, domainB, domainC] <- pure dynDomains
      -- clean federation config
      sequence_ [Internal.deleteFedConn x y | x <- domains, y <- domains, x /= y]
      -- A is connected to B and C, but B and C are not connected to each other
      void $ Internal.createFedConn domainA $ Internal.FedConn domainB "full_search"
      void $ Internal.createFedConn domainB $ Internal.FedConn domainA "full_search"
      void $ Internal.createFedConn domainA $ Internal.FedConn domainC "full_search"
      void $ Internal.createFedConn domainC $ Internal.FedConn domainA "full_search"
      uidA <- randomUser domainA def {Internal.team = True}
      unrace
        $ bindResponse
          (API.getFederationStatus uidA [domainB, domainC])
        $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "status" `shouldMatch` "non-fully-connected"
          resp.json %. "not_connected" `shouldMatchSet` [domainB, domainC]

testFederationStatus :: HasCallStack => App ()
testFederationStatus = do
  uid <- randomUser OwnDomain def {Internal.team = True}
  federatingRemoteDomain <- asString OtherDomain
  let unknownDomain = "foobar.com"
  let invalidDomain = "c.example.com" -- has no srv record
  bindResponse
    (API.getFederationStatus uid [])
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "status" `shouldMatch` "fully-connected"

  bindResponse
    (API.getFederationStatus uid [unknownDomain])
    $ \resp -> do
      resp.status `shouldMatchInt` 400
      resp.json %. "label" `shouldMatch` "discovery-failure"

  bindResponse
    (API.getFederationStatus uid [invalidDomain])
    $ \resp -> do
      resp.status `shouldMatchInt` 422
      resp.json %. "label" `shouldMatch` "invalid-domain"

  bindResponse
    (API.getFederationStatus uid [federatingRemoteDomain])
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "status" `shouldMatch` "fully-connected"

  bindResponse
    (API.getFederationStatus uid [federatingRemoteDomain, unknownDomain])
    $ \resp -> do
      resp.status `shouldMatchInt` 400
      resp.json %. "label" `shouldMatch` "discovery-failure"

testCreateConversationFullyConnected :: HasCallStack => App ()
testCreateConversationFullyConnected = do
  let setFederationConfig =
        setField "optSettings.setFederationStrategy" "allowDynamic"
          >=> removeField "optSettings.setFederationDomainConfigs"
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {dbBrig = setFederationConfig},
      def {dbBrig = setFederationConfig},
      def {dbBrig = setFederationConfig}
    ]
    $ \dynDomains -> do
      domains@[domainA, domainB, domainC] <- pure dynDomains
      connectAllDomainsAndWaitToSync 1 domains
      [u1, u2, u3] <- createAndConnectUsers [domainA, domainB, domainC]
      bindResponse (postConversation u1 (defProteus {qualifiedUsers = [u2, u3]})) $ \resp -> do
        resp.status `shouldMatchInt` 201

testCreateConversationNonFullyConnected :: HasCallStack => App ()
testCreateConversationNonFullyConnected = do
  let setFederationConfig =
        setField "optSettings.setFederationStrategy" "allowDynamic"
          >=> removeField "optSettings.setFederationDomainConfigs"
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {dbBrig = setFederationConfig},
      def {dbBrig = setFederationConfig},
      def {dbBrig = setFederationConfig}
    ]
    $ \dynDomains -> do
      domains@[domainA, domainB, domainC] <- pure dynDomains
      connectAllDomainsAndWaitToSync 1 domains
      [u1, u2, u3] <- createAndConnectUsers [domainA, domainB, domainC]
      -- stop federation between B and C
      void $ Internal.deleteFedConn domainB domainC
      void $ Internal.deleteFedConn domainC domainB
      bindResponse (postConversation u1 (defProteus {qualifiedUsers = [u2, u3]})) $ \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "non_federating_backends" `shouldMatchSet` [domainB, domainC]
