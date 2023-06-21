module Test.Conversation where

import qualified API.BrigInternal as Internal
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
              setField "optSettings.setFederationStrategy" "allowDynamic"
                >=> removeField "optSettings.setFederationDomainConfigs"
          }
  startDynamicBackends [overrides, overrides, overrides] $
    \dynDomains -> do
      domains@[domainA, domainB, domainC] <- pure dynDomains
      -- clean federation config
      sequence_ [Internal.deleteFederationRemotes x y | x <- domains, y <- domains]
      uidA <- randomUser domainA def {Internal.team = True}
      unrace
        $ bindResponse
          (API.getFederationStatus uidA [domainB, domainC])
        $ \resp -> do
          resp.status `shouldMatchInt` 400
          resp.json %. "label" `shouldMatch` "federation-denied"

testDynamicBackendsFullyConnectedWhenAllowDynamic :: HasCallStack => App ()
testDynamicBackendsFullyConnectedWhenAllowDynamic = do
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
      sequence_ [Internal.addFederationRemotes x y | x <- domains, y <- domains]
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
      sequence_ [Internal.deleteFederationRemotes x y | x <- domains, y <- domains]
      -- A is connected to B and C, but B and C are not connected to each other
      Internal.addFederationRemotes domainA domainB
      Internal.addFederationRemotes domainB domainA
      Internal.addFederationRemotes domainA domainC
      Internal.addFederationRemotes domainC domainA
      uidA <- randomUser domainA def {Internal.team = True}
      unrace
        $ bindResponse
          (API.getFederationStatus uidA [domainB, domainC])
        $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "status" `shouldMatch` "non-fully-connected"
          resp.json %. "not_connected.domains" `shouldMatchSet` [domainB, domainC]

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
