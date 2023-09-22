{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Conversation where

import API.Brig
import API.BrigInternal
import API.Galley
import API.GalleyInternal
import Control.Applicative
import Control.Concurrent
import Control.Lens hiding ((.=))
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.Aeson qualified as Aeson
import Data.ProtoLens qualified as Proto
import Data.Text qualified as T
import Data.Time
import GHC.Stack
import Notifications
import Numeric.Lens
import Proto.Otr as Proto
import SetupHelpers hiding (deleteUser)
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
    bindResponse (addMembers u1 cid def {users = members}) $ \resp -> do
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
    bindResponse (addMembers u1 cid def {users = members}) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "non_federating_backends" `shouldMatchSet` [domainB, domainC]

testAddMember :: HasCallStack => App ()
testAddMember = do
  alice <- randomUser OwnDomain def
  aliceId <- alice %. "qualified_id"
  -- create conversation with no users
  cid <- postConversation alice defProteus >>= getJSON 201
  bob <- randomUser OwnDomain def
  bobId <- bob %. "qualified_id"
  let addMember = addMembers alice cid def {role = Just "wire_member", users = [bobId]}
  bindResponse addMember $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "not-connected"
  connectUsers alice bob
  bindResponse addMember $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "type" `shouldMatch` "conversation.member-join"
    resp.json %. "qualified_from" `shouldMatch` objQidObject alice
    resp.json %. "qualified_conversation" `shouldMatch` objQidObject cid
    users <- resp.json %. "data.users" >>= asList
    addedUsers <- forM users (%. "qualified_id")
    addedUsers `shouldMatchSet` [bobId]

  -- check that both users can see the conversation
  bindResponse (getConversation alice cid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    mems <- resp.json %. "members.others" & asList
    mem <- assertOne mems
    mem %. "qualified_id" `shouldMatch` bobId
    mem %. "conversation_role" `shouldMatch` "wire_member"

  bindResponse (getConversation bob cid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    mems <- resp.json %. "members.others" & asList
    mem <- assertOne mems
    mem %. "qualified_id" `shouldMatch` aliceId
    mem %. "conversation_role" `shouldMatch` "wire_admin"

testAddMemberV1 :: HasCallStack => Domain -> App ()
testAddMemberV1 domain = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, domain]
  conv <- postConversation alice defProteus >>= getJSON 201
  bobId <- bob %. "qualified_id"
  let opts =
        def
          { version = Just 1,
            role = Just "wire_member",
            users = [bobId]
          }
  bindResponse (addMembers alice conv opts) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "type" `shouldMatch` "conversation.member-join"
    resp.json %. "qualified_from" `shouldMatch` objQidObject alice
    resp.json %. "qualified_conversation" `shouldMatch` objQidObject conv
    users <- resp.json %. "data.users" >>= asList
    traverse (%. "qualified_id") users `shouldMatchSet` [bobId]

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
  bindResponse (addMembers alex conv def {users = [bobId]}) $ \resp -> do
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
  bindResponse (addMembers alex conv def {users = [charlieId]}) $ \resp -> do
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
    bindResponse (addMembers alice conv def {users = [bobId, charlieId]}) $ \resp -> do
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

    (user, _, _) <- createTeam OwnDomain 1
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
      (user, _, _) <- createTeam domain 1
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
  bindResponse (addMembers alice conv def {users = [alex]}) $ \resp -> do
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
      nameNotif <- awaitNotification charlie client noValue 2 isConvNameChangeNotif
      nameNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      nameNotif %. "payload.0.data.name" `shouldMatch` newConvName
      leaveNotif <- awaitNotification charlie client noValue 2 isConvLeaveNotif
      leaveNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv

testConvRenaming :: HasCallStack => App ()
testConvRenaming = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  conv <-
    postConversation alice (defProteus {qualifiedUsers = [bob]})
      >>= getJSON 201
  let newConvName = "The new conversation name"
  withWebSockets [alice, bob] $ \wss -> do
    for_ wss $ \ws -> do
      void $ changeConversationName alice conv newConvName >>= getBody 200
      nameNotif <- awaitMatch 10 isConvNameChangeNotif ws
      nameNotif %. "payload.0.data.name" `shouldMatch` newConvName
      nameNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv

testReceiptModeWithRemotesOk :: HasCallStack => App ()
testReceiptModeWithRemotesOk = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  conv <-
    postConversation alice (defProteus {qualifiedUsers = [bob]})
      >>= getJSON 201
  withWebSockets [alice, bob] $ \wss -> do
    void $ updateReceiptMode alice conv (43 :: Int) >>= getBody 200
    for_ wss $ \ws -> do
      notif <- awaitMatch 10 isReceiptModeUpdateNotif ws
      notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice
      notif %. "payload.0.data.receipt_mode" `shouldMatchInt` 43

testReceiptModeWithRemotesUnreachable :: HasCallStack => App ()
testReceiptModeWithRemotesUnreachable = do
  ownDomain <- asString OwnDomain
  alice <- randomUser ownDomain def
  conv <- startDynamicBackends [mempty] $ \[dynBackend] -> do
    bob <- randomUser dynBackend def
    connectUsers alice bob
    postConversation alice (defProteus {qualifiedUsers = [bob]})
      >>= getJSON 201
  withWebSocket alice $ \ws -> do
    void $ updateReceiptMode alice conv (43 :: Int) >>= getBody 200
    notif <- awaitMatch 10 isReceiptModeUpdateNotif ws
    notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
    notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice
    notif %. "payload.0.data.receipt_mode" `shouldMatchInt` 43

testDeleteLocalMember :: HasCallStack => App ()
testDeleteLocalMember = do
  [alice, alex, bob] <- createAndConnectUsers [OwnDomain, OwnDomain, OtherDomain]
  conv <-
    postConversation alice (defProteus {qualifiedUsers = [alex, bob]})
      >>= getJSON 201
  bindResponse (removeMember alice conv alex) $ \resp -> do
    r <- getJSON 200 resp
    r %. "type" `shouldMatch` "conversation.member-leave"
    r %. "qualified_conversation" `shouldMatch` objQidObject conv
    r %. "qualified_from" `shouldMatch` objQidObject alice
    r %. "data.qualified_user_ids.0" `shouldMatch` objQidObject alex
  -- Now that Alex is gone, try removing her once again
  bindResponse (removeMember alice conv alex) $ \r -> do
    r.status `shouldMatchInt` 204
    r.jsonBody `shouldMatch` (Nothing @Aeson.Value)

testDeleteRemoteMember :: HasCallStack => App ()
testDeleteRemoteMember = do
  [alice, alex, bob] <- createAndConnectUsers [OwnDomain, OwnDomain, OtherDomain]
  conv <-
    postConversation alice (defProteus {qualifiedUsers = [alex, bob]})
      >>= getJSON 201
  bindResponse (removeMember alice conv bob) $ \resp -> do
    r <- getJSON 200 resp
    r %. "type" `shouldMatch` "conversation.member-leave"
    r %. "qualified_conversation" `shouldMatch` objQidObject conv
    r %. "qualified_from" `shouldMatch` objQidObject alice
    r %. "data.qualified_user_ids.0" `shouldMatch` objQidObject bob
  -- Now that Bob is gone, try removing him once again
  bindResponse (removeMember alice conv bob) $ \r -> do
    r.status `shouldMatchInt` 204
    r.jsonBody `shouldMatch` (Nothing @Aeson.Value)

testDeleteRemoteMemberRemoteUnreachable :: HasCallStack => App ()
testDeleteRemoteMemberRemoteUnreachable = do
  [alice, bob, bart] <- createAndConnectUsers [OwnDomain, OtherDomain, OtherDomain]
  conv <- startDynamicBackends [mempty] $ \[dynBackend] -> do
    charlie <- randomUser dynBackend def
    connectUsers alice charlie
    postConversation
      alice
      (defProteus {qualifiedUsers = [bob, bart, charlie]})
      >>= getJSON 201
  void $ withWebSockets [alice, bob] $ \wss -> do
    void $ removeMember alice conv bob >>= getBody 200
    for wss $ \ws -> do
      leaveNotif <- awaitMatch 10 isConvLeaveNotif ws
      leaveNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      leaveNotif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice
      leaveNotif %. "payload.0.data.qualified_user_ids.0" `shouldMatch` objQidObject bob
  -- Now that Bob is gone, try removing him once again
  bindResponse (removeMember alice conv bob) $ \r -> do
    r.status `shouldMatchInt` 204
    r.jsonBody `shouldMatch` (Nothing @Aeson.Value)

testDeleteTeamConversationWithRemoteMembers :: HasCallStack => App ()
testDeleteTeamConversationWithRemoteMembers = do
  (alice, team, _) <- createTeam OwnDomain 1
  conv <- postConversation alice (defProteus {team = Just team}) >>= getJSON 201
  bob <- randomUser OtherDomain def
  connectUsers alice bob
  mem <- bob %. "qualified_id"
  void $ addMembers alice conv def {users = [mem]} >>= getBody 200

  void $ withWebSockets [alice, bob] $ \wss -> do
    void $ deleteTeamConversation team conv alice >>= getBody 200
    for wss $ \ws -> do
      notif <- awaitMatch 10 isConvDeleteNotif ws
      notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice

testDeleteTeamConversationWithUnreachableRemoteMembers :: HasCallStack => App ()
testDeleteTeamConversationWithUnreachableRemoteMembers = do
  resourcePool <- asks resourcePool
  (alice, team, _) <- createTeam OwnDomain 1
  conv <- postConversation alice (defProteus {team = Just team}) >>= getJSON 201

  let assertNotification :: (HasCallStack, MakesValue n) => n -> App ()
      assertNotification notif = do
        notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
        notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice

  runCodensity (acquireResources 1 resourcePool) $ \[dynBackend] -> do
    (bob, bobClient) <- runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
      -- FUTUREWORK: get rid of this once the background worker is able to listen to all queues
      do
        ownDomain <- make OwnDomain & asString
        otherDomain <- make OtherDomain & asString
        let domains = [ownDomain, otherDomain, dynBackend.berDomain]
        sequence_
          [ createFedConn x (FedConn y "full_search")
            | x <- domains,
              y <- domains,
              x /= y
          ]

      bob <- randomUser dynBackend.berDomain def
      bobClient <- objId $ bindResponse (addClient bob def) $ getJSON 201
      connectUsers alice bob
      mem <- bob %. "qualified_id"
      void $ addMembers alice conv def {users = [mem]} >>= getBody 200
      pure (bob, bobClient)
    withWebSocket alice $ \ws -> do
      void $ deleteTeamConversation team conv alice >>= getBody 200
      notif <- awaitMatch 10 isConvDeleteNotif ws
      assertNotification notif
    void $ runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
      notif <- awaitNotification bob bobClient noValue 2 isConvDeleteNotif
      assertNotification notif

testLeaveConversationSuccess :: HasCallStack => App ()
testLeaveConversationSuccess = do
  [alice, bob, chad, dee] <-
    createAndConnectUsers [OwnDomain, OwnDomain, OtherDomain, OtherDomain]
  [aClient, bClient] <- forM [alice, bob] $ \user ->
    objId $ bindResponse (addClient user def) $ getJSON 201
  let overrides =
        def {brigCfg = setField "optSettings.setFederationStrategy" "allowAll"}
  startDynamicBackends [overrides] $ \[dynDomain] -> do
    eve <- randomUser dynDomain def
    eClient <- objId $ bindResponse (addClient eve def) $ getJSON 201
    connectUsers alice eve
    conv <-
      postConversation
        alice
        ( defProteus
            { qualifiedUsers = [bob, chad, dee, eve]
            }
        )
        >>= getJSON 201
    void $ removeMember chad conv chad >>= getBody 200
    assertLeaveNotification chad conv alice aClient chad
    assertLeaveNotification chad conv bob bClient chad
    assertLeaveNotification chad conv eve eClient chad

testOnUserDeletedConversations :: HasCallStack => App ()
testOnUserDeletedConversations = do
  let overrides =
        def {brigCfg = setField "optSettings.setFederationStrategy" "allowAll"}
  startDynamicBackends [overrides] $ \[dynDomain] -> do
    [ownDomain, otherDomain] <- forM [OwnDomain, OtherDomain] asString
    [alice, alex, bob, bart, chad] <-
      createAndConnectUsers [ownDomain, ownDomain, otherDomain, otherDomain, dynDomain]
    bobId <- bob %. "qualified_id"
    ooConvId <- do
      l <- getAllConvs alice
      let isWith users c = do
            t <- (==) <$> (c %. "type" & asInt) <*> pure 2
            others <- c %. "members.others" & asList
            qIds <- for others (%. "qualified_id")
            pure $ qIds == users && t
      c <- head <$> filterM (isWith [bobId]) l
      c %. "qualified_id"

    mainConvBefore <-
      postConversation alice (defProteus {qualifiedUsers = [alex, bob, bart, chad]})
        >>= getJSON 201

    void $ withWebSocket alex $ \ws -> do
      void $ deleteUser bob >>= getBody 200
      n <- awaitMatch 10 isConvLeaveNotif ws
      n %. "payload.0.qualified_from" `shouldMatch` bobId
      n %. "payload.0.qualified_conversation" `shouldMatch` (mainConvBefore %. "qualified_id")

      do
        -- Bob is not in the one-to-one conversation with Alice any more
        conv <- getConversation alice ooConvId >>= getJSON 200
        shouldBeEmpty $ conv %. "members.others"
      do
        -- Bob is not in the main conversation any more
        mainConvAfter <- getConversation alice (mainConvBefore %. "qualified_id") >>= getJSON 200
        mems <- mainConvAfter %. "members.others" & asList
        memIds <- for mems (%. "qualified_id")
        expectedIds <- for [alex, bart, chad] (%. "qualified_id")
        memIds `shouldMatchSet` expectedIds

testUpdateConversationByRemoteAdmin :: HasCallStack => App ()
testUpdateConversationByRemoteAdmin = do
  [alice, bob, charlie] <- createAndConnectUsers [OwnDomain, OtherDomain, OtherDomain]
  conv <-
    postConversation alice (defProteus {qualifiedUsers = [bob, charlie]})
      >>= getJSON 201
  void $ updateRole alice bob "wire_admin" (conv %. "qualified_id") >>= getBody 200
  void $ withWebSockets [alice, bob, charlie] $ \wss -> do
    void $ updateReceiptMode bob conv (41 :: Int) >>= getBody 200
    for_ wss $ \ws -> awaitMatch 10 isReceiptModeUpdateNotif ws

testSendMessageWhileThirdBackendUnreachable :: HasCallStack => App ()
testSendMessageWhileThirdBackendUnreachable = do
  let overrides = def {brigCfg = setField "optSettings.setFederationStrategy" "allowAll"}
  startDynamicBackends [overrides, overrides, overrides] $ \dynDomains -> do
    [a, b, c] <- pure dynDomains

    -- set up users and clients
    [alice, bob, charlie] <- createAndConnectUsers [a, b, c]
    [aClient, bClient, cClient] <- forM [alice, bob, charlie] $ \user ->
      objId $ bindResponse (addClient user def) $ getJSON 201

    -- set up conversations between A+B
    convAB <- postConversation alice defProteus >>= getJSON 201
    void $ addMembers alice convAB (def {users = [bob]}) >>= getBody 200
    -- set up conversation between A+B+C where C is a black hole
    convBlackHole <- postConversation alice defProteus >>= getJSON 201
    void $ addMembers alice convBlackHole (def {users = [bob, charlie]}) >>= getBody 200

    -- send msg in AB should work
    do
      msg <- mkProteusRecipient bob bClient "a message for Bob"
      t <- time $ bindResponse (postProteusMessage alice convAB (successfulMsg aClient [msg])) assertSuccess
      putStrLn $ "send msg to AB fist time took: " <> show t

    -- send msg in black hole async
    do
      msgBob <- mkProteusRecipient bob bClient "a message for Bob"
      msgCharlie <- mkProteusRecipient charlie cClient "a message for Charlie"
      sendMsgAction <- appToIO $ bindResponse (postProteusMessage alice convBlackHole (successfulMsg aClient [msgBob, msgCharlie])) assertSuccess
      let sendMsgAsyncWithTimer :: App () = void $ liftIO $ forkIO $ do
            t <- time sendMsgAction
            putStrLn $ "Black hole request took: " <> show t
      -- send to black hole 5 x
      replicateM_ 5 sendMsgAsyncWithTimer

    -- send a msg in AB again but wait a little bit (20s)
    liftIO $ threadDelay $ 20 * 1000 * 1000
    do
      msg <- mkProteusRecipient bob bClient "a message for Bob"
      t <- time $ bindResponse (postProteusMessage alice convAB (successfulMsg aClient [msg])) assertSuccess
      putStrLn $ "send msg to AB second time took: " <> show t

    -- wait for 2 minutes
    liftIO $ threadDelay $ 2 * 60 * 1000 * 1000
  where
    successfulMsg :: String -> [Proto.QualifiedUserEntry] -> Proto.QualifiedNewOtrMessage
    successfulMsg sender msgs =
      Proto.defMessage @Proto.QualifiedNewOtrMessage
        & #sender . #client .~ (sender ^?! hex)
        & #recipients .~ msgs
        & #reportAll .~ Proto.defMessage

    time :: (Monad m, MonadIO m) => m () -> m NominalDiffTime
    time action = do
      start <- liftIO getCurrentTime
      void action
      end <- liftIO getCurrentTime
      pure $ diffUTCTime end start
