module Test.Defederation where

import API.BrigInternal
import API.BrigInternal qualified as Internal
import API.Galley (defProteus, getConversation, postConversation, qualifiedUsers)
import Control.Applicative
import Data.Aeson qualified as Aeson
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testDefederationRemoteNotifications :: HasCallStack => App ()
testDefederationRemoteNotifications = do
  let remoteDomain = "example.example.com"
  -- Setup federation between OtherDomain and the remote domain
  void $ createFedConn OtherDomain (FedConn remoteDomain "full_search")

  -- Setup a remote user we can get notifications for.
  user <- randomUser OtherDomain def

  withWebSocket user $ \ws -> do
    -- Defederate from a domain that doesn't exist. This won't do anything to the databases
    -- But it will send out notifications that we can wait on.
    -- Begin the whole process at Brig, the same as an operator would.
    void $ deleteFedConn OwnDomain remoteDomain
    void $ awaitNMatches 2 3 (\n -> nPayload n %. "type" `isEqual` "federation.connectionRemoved") ws

testDefederationNonFullyConnectedGraph :: HasCallStack => App ()
testDefederationNonFullyConnectedGraph = do
  let setFederationConfig =
        setField "optSettings.setFederationStrategy" "allowDynamic"
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig}
    ]
    $ \dynDomains -> do
      domains@[domainA, domainB, domainC] <- pure dynDomains
      connectAllDomainsAndWaitToSync 1 domains

      -- create a few extra users and connections to make sure that does not lead to any extra `connectionRemoved` notifications
      [uA, uA2, _, _, uB, uC] <- createAndConnectUsers [domainA, domainA, domainA, domainA, domainB, domainC] >>= traverse objQidObject

      -- create group conversation owned by domainA with users from domainB and domainC
      convId <- bindResponse (postConversation uA (defProteus {qualifiedUsers = [uA2, uB, uC]})) $ \r -> do
        r.status `shouldMatchInt` 201
        r.json %. "qualified_id"

      -- check conversation exists on all backends
      checkConv convId uA [uB, uC, uA2]
      checkConv convId uB [uA, uC, uA2]
      checkConv convId uC [uA, uB, uA2]

      withWebSocket uA $ \wsA -> do
        -- one of the 2 non-conversation-owning domains (domainB and domainC)
        -- defederate from the other non-conversation-owning domain
        void $ Internal.deleteFedConn domainB domainC

        -- assert that clients from domainA receive federation.connectionRemoved events
        -- Notifications being delivered exactly twice
        void $ awaitNMatches 2 20 (isConnectionRemoved [domainB, domainC]) wsA

        -- remote members should be removed from local conversation eventually
        retryT $ checkConv convId uA [uA2]
  where
    isConnectionRemoved :: [String] -> Value -> App Bool
    isConnectionRemoved domains n = do
      correctType <- nPayload n %. "type" `isEqual` "federation.connectionRemoved"
      if correctType
        then do
          domsV <- nPayload n %. "domains" & asList
          domsStr <- for domsV asString <&> sort
          pure $ domsStr == sort domains
        else pure False

    checkConv :: Value -> Value -> [Value] -> App ()
    checkConv convId user expectedOtherMembers = do
      bindResponse (getConversation user convId) $ \r -> do
        r.status `shouldMatchInt` 200
        members <- r.json %. "members.others" & asList
        qIds <- for members (\m -> m %. "qualified_id")
        qIds `shouldMatchSet` expectedOtherMembers
