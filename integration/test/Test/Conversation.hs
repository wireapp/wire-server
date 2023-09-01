{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Conversation where

import API.Brig
import API.BrigInternal
import API.Galley
import API.GalleyInternal
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import GHC.Stack
import Notifications
import SetupHelpers
import Testlib.One2One (generateRemoteAndConvIdWithDomain)
import Testlib.Prelude
import Testlib.ResourcePool

testDynamicBackendsFullyConnectedWhenAllowAll :: HasCallStack => App ()
testDynamicBackendsFullyConnectedWhenAllowAll = do
  let overrides =
        def {brigCfg = setField "optSettings.setFederationStrategy" "allowAll"}
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
          { brigCfg =
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
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {brigCfg = overrides},
      def {brigCfg = overrides},
      def {brigCfg = overrides}
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
          { brigCfg =
              setField "optSettings.setFederationStrategy" "allowDynamic"
                >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
          }
  startDynamicBackends [overrides, overrides, overrides] $
    \[domainA, domainB, domainC] -> do
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
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig}
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
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig}
    ]
    $ \dynDomains -> do
      [domainA, domainB, domainC] <- pure dynDomains

      -- A is connected to B and C, but B and C are not connected to each other
      void $ createFedConn domainA $ FedConn domainB "full_search"
      void $ createFedConn domainB $ FedConn domainA "full_search"
      void $ createFedConn domainA $ FedConn domainC "full_search"
      void $ createFedConn domainC $ FedConn domainA "full_search"
      liftIO $ threadDelay (2 * 1000 * 1000)

      u1 <- randomUser domainA def
      u2 <- randomUser domainB def
      u3 <- randomUser domainC def
      connectUsers u1 u2
      connectUsers u1 u3

      bindResponse (postConversation u1 (defProteus {qualifiedUsers = [u2, u3]})) $ \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "non_federating_backends" `shouldMatchSet` [domainB, domainC]

testAddMembersFullyConnectedProteus :: HasCallStack => App ()
testAddMembersFullyConnectedProteus = do
  withFederatingBackendsAllowDynamic $ \(domainA, domainB, domainC) -> do
    connectAllDomainsAndWaitToSync 2 [domainA, domainB, domainC]
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
  withFederatingBackendsAllowDynamic $ \(domainA, domainB, domainC) -> do
    void $ createFedConn domainA (FedConn domainB "full_search")
    void $ createFedConn domainB (FedConn domainA "full_search")
    void $ createFedConn domainA (FedConn domainC "full_search")
    void $ createFedConn domainC (FedConn domainA "full_search")
    liftIO $ threadDelay (2 * 1000 * 1000) -- wait for federation status to be updated

    -- add users
    u1 <- randomUser domainA def
    u2 <- randomUser domainB def
    u3 <- randomUser domainC def
    connectUsers u1 u2
    connectUsers u1 u3

    -- create conversation with no users
    cid <- postConversation u1 (defProteus {qualifiedUsers = []}) >>= getJSON 201
    -- add members from remote backends
    members <- for [u2, u3] (%. "qualified_id")
    bindResponse (addMembers u1 cid members) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "non_federating_backends" `shouldMatchSet` [domainB, domainC]

testConvWithUnreachableRemoteUsers :: HasCallStack => App ()
testConvWithUnreachableRemoteUsers = do
  let overrides =
        def {brigCfg = setField "optSettings.setFederationStrategy" "allowAll"}
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
        def {brigCfg = setField "optSettings.setFederationStrategy" "allowAll"}
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
        def {brigCfg = setField "optSettings.setFederationStrategy" "allowAll"}
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
          { brigCfg =
              setField "optSettings.setFederationStrategy" "allowDynamic"
          }
  startDynamicBackends [overrides] $ \[dynBackend] -> do
    own <- asString OwnDomain
    other <- asString OtherDomain

    -- Ensure that dynamic backend only federates with own domain, but not other
    -- domain.
    void $ createFedConn dynBackend (FedConn own "full_search")

    alice <- randomUser own def
    bob <- randomUser other def
    charlie <- randomUser dynBackend def
    -- We use retryT here so the dynamic federated connection changes can take
    -- some time to be propagated. Remove after fixing https://wearezeta.atlassian.net/browse/WPB-3797
    mapM_ (retryT . connectUsers alice) [bob, charlie]

    let newConv = defProteus {qualifiedUsers = []}
    conv <- postConversation alice newConv >>= getJSON 201

    bobId <- bob %. "qualified_id"
    charlieId <- charlie %. "qualified_id"
    bindResponse (addMembers alice conv [bobId, charlieId]) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "non_federating_backends" `shouldMatchSet` [other, dynBackend]

testGetOneOnOneConvInStatusSentFromRemote :: App ()
testGetOneOnOneConvInStatusSentFromRemote = do
  d1User <- randomUser OwnDomain def
  let shouldBeLocal = True
  (d2Usr, d2ConvId) <- generateRemoteAndConvIdWithDomain OtherDomain (not shouldBeLocal) d1User
  bindResponse (postConnection d1User d2Usr) $ \r -> do
    r.status `shouldMatchInt` 201
    r.json %. "status" `shouldMatch` "sent"
  bindResponse (listConversationIds d1User def) $ \r -> do
    r.status `shouldMatchInt` 200
    convIds <- r.json %. "qualified_conversations" & asList
    filter ((==) d2ConvId) convIds `shouldMatch` [d2ConvId]
  bindResponse (getConnections d1User) $ \r -> do
    qConvIds <- r.json %. "connections" & asList >>= traverse (%. "qualified_conversation")
    filter ((==) d2ConvId) qConvIds `shouldMatch` [d2ConvId]
  resp <- getConversation d1User d2ConvId
  resp.status `shouldMatchInt` 200

testMultiIngressGuestLinks :: HasCallStack => App ()
testMultiIngressGuestLinks = do
  do
    configuredURI <- readServiceConfig Galley & (%. "settings.conversationCodeURI") & asText

    (user, _) <- createTeam OwnDomain
    conv <- postConversation user (allowGuests defProteus) >>= getJSON 201

    bindResponse (postConversationCode user conv Nothing Nothing) $ \resp -> do
      res <- getJSON 201 resp
      res %. "type" `shouldMatch` "conversation.code-update"
      guestLink <- res %. "data.uri" & asText
      assertBool "guestlink incorrect" $ configuredURI `T.isPrefixOf` guestLink

    bindResponse (getConversationCode user conv Nothing) $ \resp -> do
      res <- getJSON 200 resp
      guestLink <- res %. "uri" & asText
      assertBool "guestlink incorrect" $ configuredURI `T.isPrefixOf` guestLink

    bindResponse (getConversationCode user conv (Just "red.example.com")) $ \resp -> do
      res <- getJSON 200 resp
      guestLink <- res %. "uri" & asText
      assertBool "guestlink incorrect" $ configuredURI `T.isPrefixOf` guestLink

  withModifiedBackend
    ( def
        { galleyCfg = \conf ->
            conf
              & setField "settings.conversationCodeURI" Null
              & setField
                "settings.multiIngress"
                ( object
                    [ "red.example.com" .= "https://red.example.com",
                      "blue.example.com" .= "https://blue.example.com"
                    ]
                )
        }
    )
    $ \domain -> do
      (user, _) <- createTeam domain
      conv <- postConversation user (allowGuests defProteus) >>= getJSON 201

      bindResponse (postConversationCode user conv Nothing (Just "red.example.com")) $ \resp -> do
        res <- getJSON 201 resp
        res %. "type" `shouldMatch` "conversation.code-update"
        guestLink <- res %. "data.uri" & asText
        assertBool "guestlink incorrect" $ (fromString "https://red.example.com") `T.isPrefixOf` guestLink

      bindResponse (getConversationCode user conv (Just "red.example.com")) $ \resp -> do
        res <- getJSON 200 resp
        guestLink <- res %. "uri" & asText
        assertBool "guestlink incorrect" $ (fromString "https://red.example.com") `T.isPrefixOf` guestLink

      bindResponse (getConversationCode user conv (Just "blue.example.com")) $ \resp -> do
        res <- getJSON 200 resp
        guestLink <- res %. "uri" & asText
        assertBool "guestlink incorrect" $ (fromString "https://blue.example.com") `T.isPrefixOf` guestLink

      bindResponse (getConversationCode user conv Nothing) $ \resp -> do
        res <- getJSON 403 resp
        res %. "label" `shouldMatch` "access-denied"

      bindResponse (getConversationCode user conv (Just "unknown.example.com")) $ \resp -> do
        res <- getJSON 403 resp
        res %. "label" `shouldMatch` "access-denied"

testAddUserWhenOtherBackendOffline :: HasCallStack => App ()
testAddUserWhenOtherBackendOffline = do
  let overrides =
        def {brigCfg = setField "optSettings.setFederationStrategy" "allowAll"}
  ([alice, alex], conv) <-
    startDynamicBackends [overrides] $ \domains -> do
      own <- make OwnDomain & asString
      [alice, alex, charlie] <-
        createAndConnectUsers $ [own, own] <> domains

      let newConv = defProteus {qualifiedUsers = [charlie]}
      conv <- postConversation alice newConv >>= getJSON 201
      pure ([alice, alex], conv)
  bindResponse (addMembers alice conv [alex]) $ \resp -> do
    resp.status `shouldMatchInt` 200

testSynchroniseUserRemovalNotification :: HasCallStack => App ()
testSynchroniseUserRemovalNotification = do
  resourcePool <- asks resourcePool
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  runCodensity (acquireResources 1 resourcePool) $ \[dynBackend] -> do
    (conv, charlie, client) <-
      runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
        charlie <- randomUser dynBackend.berDomain def
        client <- objId $ bindResponse (addClient charlie def) $ getJSON 201
        mapM_ (connectUsers charlie) [alice, bob]
        conv <-
          postConversation alice (defProteus {qualifiedUsers = [bob, charlie]})
            >>= getJSON 201
        pure (conv, charlie, client)

    let newConvName = "The new conversation name"
    bindResponse (changeConversationName alice conv newConvName) $ \resp ->
      resp.status `shouldMatchInt` 200
    bindResponse (removeMember alice conv charlie) $ \resp ->
      resp.status `shouldMatchInt` 200
    runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
      nameNotif <- awaitNotification charlie client noValue 2 $ isConvNameChangeNotif newConvName
      nameNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      leaveNotif <- awaitNotification charlie client noValue 2 isConvLeaveNotif
      leaveNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv

testConvRenaming :: HasCallStack => App ()
testConvRenaming = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  client <- objId $ bindResponse (addClient alice def) $ getJSON 201
  conv <-
    postConversation alice (defProteus {qualifiedUsers = [bob]})
      >>= getJSON 201
  let newConvName = "The new conversation name"
  void $ changeConversationName alice conv newConvName >>= getBody 200
  nameNotif <- awaitNotification alice client noValue 2 $ isConvNameChangeNotif newConvName
  nameNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv

testReceiptModeWithRemotesOk :: HasCallStack => App ()
testReceiptModeWithRemotesOk = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  client <- objId $ bindResponse (addClient alice def) $ getJSON 201
  conv <-
    postConversation alice (defProteus {qualifiedUsers = [bob]})
      >>= getJSON 201
  let mode43 :: Int32 = 43
  void $ updateReceiptMode alice conv mode43 >>= getBody 200
  notif <- awaitNotification alice client noValue 5 isReceiptModeUpdateNotif
  notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
  notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice
  notif %. "payload.0.data.receipt_mode" `shouldMatch` mode43

testReceiptModeWithRemotesUnreachable :: HasCallStack => App ()
testReceiptModeWithRemotesUnreachable = do
  resourcePool <- asks resourcePool
  ownDomain <- asString OwnDomain
  alice <- randomUser ownDomain def
  client <- objId $ bindResponse (addClient alice def) $ getJSON 201
  runCodensity (acquireResources 1 resourcePool) $ \[dynBackend] -> do
    conv <-
      runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
        bob <- randomUser dynBackend.berDomain def
        connectUsers alice bob
        postConversation alice (defProteus {qualifiedUsers = [bob]})
          >>= getJSON 201
    let mode43 :: Int32 = 43
    void $ updateReceiptMode alice conv mode43 >>= getBody 200
    notif <- awaitNotification alice client noValue 5 isReceiptModeUpdateNotif
    notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
    notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice
    notif %. "payload.0.data.receipt_mode" `shouldMatch` mode43
