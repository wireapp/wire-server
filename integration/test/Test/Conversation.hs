{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Conversation where

import API.Brig (getConnection)
import API.BrigInternal
import API.Galley
import API.GalleyInternal
import API.Gundeck (getNotifications)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Data.Aeson qualified as Aeson
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
    uidA <- randomUser domainA def {team = True}
    uidB <- randomUser domainA def {team = True}
    uidC <- randomUser domainA def {team = True}
    assertConnected uidA domainB domainC
    assertConnected uidB domainA domainC
    assertConnected uidC domainA domainB
  where
    assertConnected :: (HasCallStack, MakesValue user) => user -> String -> String -> App ()
    assertConnected u d d' =
      bindResponse
        (getFederationStatus u [d, d'])
        $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "status" `shouldMatch` "fully-connected"

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
      uidA <- randomUser domainA def {team = True}
      retryT
        $ bindResponse
          (getFederationStatus uidA [domainB, domainC])
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
      sequence_ [createFedConn x (FedConn y "full_search") | x <- domains, y <- domains, x /= y]
      uidA <- randomUser domainA def {team = True}
      uidB <- randomUser domainB def {team = True}
      uidC <- randomUser domainC def {team = True}
      let assertConnected u d d' =
            bindResponse
              (getFederationStatus u [d, d'])
              $ \resp -> do
                resp.status `shouldMatchInt` 200
                resp.json %. "status" `shouldMatch` "fully-connected"
      retryT $ assertConnected uidA domainB domainC
      retryT $ assertConnected uidB domainA domainC
      retryT $ assertConnected uidC domainA domainB

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
      sequence_ [deleteFedConn x y | x <- domains, y <- domains, x /= y]
      -- A is connected to B and C, but B and C are not connected to each other
      void $ createFedConn domainA $ FedConn domainB "full_search"
      void $ createFedConn domainB $ FedConn domainA "full_search"
      void $ createFedConn domainA $ FedConn domainC "full_search"
      void $ createFedConn domainC $ FedConn domainA "full_search"
      uidA <- randomUser domainA def {team = True}
      retryT
        $ bindResponse
          (getFederationStatus uidA [domainB, domainC])
        $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "status" `shouldMatch` "non-fully-connected"
          resp.json %. "not_connected" `shouldMatchSet` [domainB, domainC]

testFederationStatus :: HasCallStack => App ()
testFederationStatus = do
  uid <- randomUser OwnDomain def {team = True}
  federatingRemoteDomain <- asString OtherDomain
  let invalidDomain = "c.example.com" -- Does not have any srv records
  bindResponse
    (getFederationStatus uid [])
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "status" `shouldMatch` "fully-connected"

  bindResponse
    (getFederationStatus uid [invalidDomain])
    $ \resp -> do
      resp.status `shouldMatchInt` 422
      resp.json %. "label" `shouldMatch` "invalid-domain"

  bindResponse
    (getFederationStatus uid [federatingRemoteDomain])
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "status" `shouldMatch` "fully-connected"

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
      void $ deleteFedConn domainB domainC
      void $ deleteFedConn domainC domainB
      liftIO $ threadDelay (2 * 1000 * 1000)
      bindResponse (postConversation u1 (defProteus {qualifiedUsers = [u2, u3]})) $ \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "non_federating_backends" `shouldMatchSet` [domainB, domainC]

testDefederationGroupConversation :: HasCallStack => App ()
testDefederationGroupConversation = do
  let setFederationConfig =
        setField "optSettings.setFederationStrategy" "allowDynamic"
          >=> removeField "optSettings.setFederationDomainConfigs"
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {dbBrig = setFederationConfig},
      def {dbBrig = setFederationConfig}
    ]
    $ \dynDomains -> do
      domains@[domainA, domainB] <- pure dynDomains
      connectAllDomainsAndWaitToSync 1 domains
      [uA, uB] <- createAndConnectUsers [domainA, domainB]
      withWebSocket uA $ \ws -> do
        -- create group conversation owned by domainB
        convId <- bindResponse (postConversation uB (defProteus {qualifiedUsers = [uA]})) $ \r -> do
          r.status `shouldMatchInt` 201
          r.json %. "qualified_id"

        -- check conversation exists and uB is a member from POV of uA
        bindResponse (getConversation uA convId) $ \r -> do
          r.status `shouldMatchInt` 200
          members <- r.json %. "members.others" & asList
          qIds <- for members (\m -> m %. "qualified_id")
          uBQId <- objQidObject uB
          qIds `shouldMatchSet` [uBQId]

        -- check conversation exists and uA is a member from POV of uB
        bindResponse (getConversation uB convId) $ \r -> do
          r.status `shouldMatchInt` 200
          members <- r.json %. "members.others" & asList
          qIds <- for members (\m -> m %. "qualified_id")
          uAQId <- objQidObject uA
          qIds `shouldMatchSet` [uAQId]

        -- domainA stops federating with domainB
        void $ deleteFedConn domainA domainB

        -- assert conversation deleted from domainA
        retryT $
          bindResponse (getConversation uA convId) $ \r ->
            r.status `shouldMatchInt` 404

        -- assert federation.delete event is sent twice
        void $ awaitNMatches 2 3 (\n -> nPayload n %. "type" `isEqual` "federation.delete") ws

      -- assert no conversation.delete event is sent to uA
      eventPayloads <-
        getNotifications uA "cA" def
          >>= getJSON 200
          >>= \n -> n %. "notifications" & asList >>= \ns -> for ns nPayload

      forM_ eventPayloads $ \p ->
        p %. "type" `shouldNotMatch` "conversation.delete"

testDefederationOneOnOne :: HasCallStack => App ()
testDefederationOneOnOne = do
  let setFederationConfig =
        setField "optSettings.setFederationStrategy" "allowDynamic"
          >=> removeField "optSettings.setFederationDomainConfigs"
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {dbBrig = setFederationConfig},
      def {dbBrig = setFederationConfig}
    ]
    $ \dynDomains -> do
      domains@[domainA, domainB] <- pure dynDomains
      connectAllDomainsAndWaitToSync 1 domains
      [uA, uB] <- createAndConnectUsers [domainA, domainB]
      -- figure out on which backend the 1:1 conversation is created
      qConvId <- getConnection uA uB >>= \c -> c.json %. "qualified_conversation"

      -- check conversation exists and uB is a member from POV of uA
      bindResponse (getConversation uA qConvId) $ \r -> do
        r.status `shouldMatchInt` 200
        members <- r.json %. "members.others" & asList
        qIds <- for members (\m -> m %. "qualified_id")
        uBQId <- objQidObject uB
        qIds `shouldMatchSet` [uBQId]

      -- check conversation exists and uA is a member from POV of uB
      bindResponse (getConversation uB qConvId) $ \r -> do
        r.status `shouldMatchInt` 200
        members <- r.json %. "members.others" & asList
        qIds <- for members (\m -> m %. "qualified_id")
        uAQId <- objQidObject uA
        qIds `shouldMatchSet` [uAQId]

      conversationOwningDomain <- objDomain qConvId

      when (domainA == conversationOwningDomain) $ do
        -- conversation is created on domainA
        assertFederationTerminatingUserNoConvDeleteEvent uB qConvId domainB domainA

      when (domainB == conversationOwningDomain) $ do
        -- conversation is created on domainB
        assertFederationTerminatingUserNoConvDeleteEvent uA qConvId domainA domainB

      when (domainA /= conversationOwningDomain && domainB /= conversationOwningDomain) $ do
        -- this should not happen
        error "impossible"
  where
    assertFederationTerminatingUserNoConvDeleteEvent :: Value -> Value -> String -> String -> App ()
    assertFederationTerminatingUserNoConvDeleteEvent user convId ownDomain otherDomain = do
      withWebSocket user $ \ws -> do
        void $ deleteFedConn ownDomain otherDomain

        -- assert conversation deleted eventually
        retryT $
          bindResponse (getConversation user convId) $ \r ->
            r.status `shouldMatchInt` 404

        -- assert federation.delete event is sent twice
        void $ awaitNMatches 2 3 (\n -> nPayload n %. "type" `isEqual` "federation.delete") ws

      -- assert no conversation.delete event is sent to uA
      eventPayloads <-
        getNotifications user "user-client" def
          >>= getJSON 200
          >>= \n -> n %. "notifications" & asList >>= \ns -> for ns nPayload

      forM_ eventPayloads $ \p ->
        p %. "type" `shouldNotMatch` "conversation.delete"

testAddMembersFullyConnectedProteus :: HasCallStack => App ()
testAddMembersFullyConnectedProteus = do
  withFederatingBackendsAllowDynamic 2 $ \(domainA, domainB, domainC) -> do
    [u1, u2, u3] <- createAndConnectUsers [domainA, domainB, domainC]
    -- create conversation with no users
    cid <- postConversation u1 (defProteus {qualifiedUsers = []}) >>= getJSON 201
    -- add members from remote backends
    members <- for [u2, u3] (%. "qualified_id")
    bindResponse (addMembers u1 cid members) $ \resp -> do
      resp.status `shouldMatchInt` 200
      users <- resp.json %. "data.users" >>= asList
      addedUsers <- forM users (%. "qualified_id")
      addedUsers `shouldMatchSet` members

testAddMembersNonFullyConnectedProteus :: HasCallStack => App ()
testAddMembersNonFullyConnectedProteus = do
  withFederatingBackendsAllowDynamic 2 $ \(domainA, domainB, domainC) -> do
    [u1, u2, u3] <- createAndConnectUsers [domainA, domainB, domainC]
    -- create conversation with no users
    cid <- postConversation u1 (defProteus {qualifiedUsers = []}) >>= getJSON 201
    -- stop federation between B and C
    void $ deleteFedConn domainB domainC
    void $ deleteFedConn domainC domainB
    liftIO $ threadDelay (2 * 1000 * 1000) -- wait for federation status to be updated
    -- add members from remote backends
    members <- for [u2, u3] (%. "qualified_id")
    bindResponse (addMembers u1 cid members) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "non_federating_backends" `shouldMatchSet` [domainB, domainC]
