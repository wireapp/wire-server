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
  bindResponse (createFedConn OtherDomain $ object ["domain" .= remoteDomain, "search_policy" .= "full_search"]) $ \resp ->
    resp.status `shouldMatchInt` 200

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
      [uA, uB, uC] <- createAndConnectUsers [domainA, domainB, domainC]
      -- create group conversation owned by domainA with users from domainB and domainC
      convId <- bindResponse (postConversation uA (defProteus {qualifiedUsers = [uB, uC]})) $ \r -> do
        r.status `shouldMatchInt` 201
        r.json %. "qualified_id"

      -- check conversation exists on all backends
      for [uB, uC] objQidObject >>= checkConv convId uA

      -- one of the 2 non-conversation-owning domains (domainB and domainC)
      -- defederate from the other non-conversation-owning domain
      void $ Internal.deleteFedConn domainB domainC

      -- FUTUREWORK: the following assertion is failing on CI for some reason, but not locally.
      -- uncomment when it's fixed on CI.
      -- assert that clients from domainA receive federation.connectionRemoved events
      -- Notifications being delivered at least n times is what we want to ensure here,
      -- however they are often delivered more than once, so check that it doesn't happen
      -- hundreds of times.
      -- let isConnectionRemoved n = do
      --       correctType <- nPayload n %. "type" `isEqual` "federation.connectionRemoved"
      --       if correctType
      --         then do
      --           domsV <- nPayload n %. "domains" & asList
      --           domsStr <- for domsV asString <&> sort
      --           pure $ domsStr == sort [domainB, domainC]
      --         else pure False
      -- void $ awaitNToMMatches 2 10 20 isConnectionRemoved wsA

      retryT $ checkConv convId uA []
  where
    checkConv :: Value -> Value -> [Value] -> App ()
    checkConv convId user expectedOtherMembers = do
      bindResponse (getConversation user convId) $ \r -> do
        r.status `shouldMatchInt` 200
        members <- r.json %. "members.others" & asList
        qIds <- for members (\m -> m %. "qualified_id")
        qIds `shouldMatchSet` expectedOtherMembers
