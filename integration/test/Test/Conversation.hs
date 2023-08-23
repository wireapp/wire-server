{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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
          resp.status `shouldMatchInt` 533
          resp.json %. "unreachable_backends" `shouldMatchSet` [domainB, domainC]

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
      resp.status `shouldMatchInt` 533
      resp.json %. "unreachable_backends" `shouldMatchSet` [invalidDomain]

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
        void $
          awaitNMatches
            2
            3
            ( \n -> do
                correctType <- nPayload n %. "type" `isEqual` "federation.delete"
                if correctType
                  then nPayload n %. "domain" `isEqual` domainB
                  else pure False
            )
            ws

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

testConvWithUnreachableRemoteUsers :: HasCallStack => App ()
testConvWithUnreachableRemoteUsers = do
  let overrides =
        def {dbBrig = setField "optSettings.setFederationStrategy" "allowAll"}
          <> fullSearchWithAll
  ([alice, alex, bob, charlie, dylan], domains) <-
    startDynamicBackends [overrides, overrides] $ \domains -> do
      own <- make OwnDomain & asString
      other <- make OtherDomain & asString
      users <- createAndConnectUsers $ [own, own, other] <> domains
      pure (users, domains)

  let newConv = defProteus {qualifiedUsers = [alex, bob, charlie, dylan]}
  bindResponse (postConversation alice newConv) $ \resp -> do
    resp.status `shouldMatchInt` 533
    resp.json %. "unreachable_backends" `shouldMatchSet` domains

  convs <- getAllConvs alice >>= asList
  regConvs <- filterM (\c -> (==) <$> (c %. "type" & asInt) <*> pure 0) convs
  regConvs `shouldMatch` ([] :: [Value])

testAddReachableWithUnreachableRemoteUsers :: HasCallStack => App ()
testAddReachableWithUnreachableRemoteUsers = do
  let overrides =
        def {dbBrig = setField "optSettings.setFederationStrategy" "allowAll"}
          <> fullSearchWithAll
  ([alex, bob], conv, domains) <-
    startDynamicBackends [overrides, overrides] $ \domains -> do
      own <- make OwnDomain & asString
      other <- make OtherDomain & asString
      [alice, alex, bob, charlie, dylan] <-
        createAndConnectUsers $ [own, own, other] <> domains

      let newConv = defProteus {qualifiedUsers = [alex, charlie, dylan]}
      conv <- postConversation alice newConv >>= getJSON 201
      pure ([alex, bob], conv, domains)

  bobId <- bob %. "qualified_id"
  bindResponse (addMembers alex conv [bobId]) $ \resp -> do
    -- This test is updated to reflect the changes in `performConversationJoin`
    -- `performConversationJoin` now does a full check between all federation members
    -- that will be in the conversation when adding users to a conversation. This is
    -- to ensure that users from domains that aren't federating are not directly
    -- connected to each other.
    resp.status `shouldMatchInt` 533
    resp.jsonBody %. "unreachable_backends" `shouldMatchSet` domains

testAddUnreachable :: HasCallStack => App ()
testAddUnreachable = do
  let overrides =
        def {dbBrig = setField "optSettings.setFederationStrategy" "allowAll"}
          <> fullSearchWithAll
  ([alex, charlie], [charlieDomain, dylanDomain], conv) <-
    startDynamicBackends [overrides, overrides] $ \domains -> do
      own <- make OwnDomain & asString
      [alice, alex, charlie, dylan] <-
        createAndConnectUsers $ [own, own] <> domains

      let newConv = defProteus {qualifiedUsers = [alex, dylan]}
      conv <- postConversation alice newConv >>= getJSON 201
      pure ([alex, charlie], domains, conv)

  charlieId <- charlie %. "qualified_id"
  bindResponse (addMembers alex conv [charlieId]) $ \resp -> do
    resp.status `shouldMatchInt` 533
    -- All of the domains that are in the conversation, or will be in the conversation,
    -- need to be reachable so we can check that the graph for those domains is fully connected.
    resp.json %. "unreachable_backends" `shouldMatchSet` [charlieDomain, dylanDomain]

testAddingUserNonFullyConnectedFederation :: HasCallStack => App ()
testAddingUserNonFullyConnectedFederation = do
  let overrides =
        def
          { dbBrig =
              setField "optSettings.setFederationStrategy" "allowDynamic"
                >=> removeField "optSettings.setFederationDomainConfigs"
          }
  startDynamicBackends [overrides] $ \[dynBackend] -> do
    own <- asString OwnDomain
    other <- asString OtherDomain

    -- Ensure that dynamic backend only federates with own domain, but not other
    -- domain.
    --
    -- FUTUREWORK: deleteAllFedConns at the time of acquiring a backend, so
    -- tests don't affect each other.
    deleteAllFedConns dynBackend
    void $ createFedConn dynBackend (FedConn own "full_search")

    alice <- randomUser own def
    bob <- randomUser other def
    charlie <- randomUser dynBackend def
    -- We use retryT here so the dynamic federated connection changes can take
    -- some time to be propagated. Remove after fixing https://wearezeta.atlassian.net/browse/WPB-3797
    mapM_ (retryT . connectUsers2 alice) [bob, charlie]

    let newConv = defProteus {qualifiedUsers = []}
    conv <- postConversation alice newConv >>= getJSON 201

    bobId <- bob %. "qualified_id"
    charlieId <- charlie %. "qualified_id"
    bindResponse (addMembers alice conv [bobId, charlieId]) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "non_federating_backends" `shouldMatchSet` [other, dynBackend]
