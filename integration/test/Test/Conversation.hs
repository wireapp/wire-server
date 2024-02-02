{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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
import qualified API.BrigInternal as BrigI
import API.Galley
import API.GalleyInternal
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Stack
import Notifications
import SetupHelpers hiding (deleteUser)
import Testlib.One2One (generateRemoteAndConvIdWithDomain)
import Testlib.Prelude
import Testlib.ResourcePool

testDynamicBackendsFullyConnectedWhenAllowAll :: HasCallStack => App ()
testDynamicBackendsFullyConnectedWhenAllowAll = do
  -- The default setting is 'allowAll'
  startDynamicBackends [def, def, def] $ \dynDomains -> do
    [domainA, domainB, domainC] <- pure dynDomains
    uidA <- randomUser domainA def {BrigI.team = True}
    uidB <- randomUser domainA def {BrigI.team = True}
    uidC <- randomUser domainA def {BrigI.team = True}
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
  startDynamicBackends [overrides, overrides, overrides] $ \[domainA, domainB, domainC] -> do
    uidA <- randomUser domainA def {BrigI.team = True}
    retryT
      $ bindResponse
        (getFederationStatus uidA [domainB, domainC])
      $ \resp -> do
        resp.status `shouldMatchInt` 533
        resp.json %. "unreachable_backends" `shouldMatchSet` [domainB, domainC]

testDynamicBackendsFullyConnectedWhenAllowDynamic :: HasCallStack => App ()
testDynamicBackendsFullyConnectedWhenAllowDynamic = do
  withFederatingBackendsAllowDynamic $ \(domainA, domainB, domainC) -> do
    -- Allowing 'full_search' or any type of search is how we enable federation
    -- between backends when the federation strategy is 'allowDynamic'.
    sequence_
      [ BrigI.createFedConn x (BrigI.FedConn y "full_search" Nothing)
        | x <- [domainA, domainB, domainC],
          y <- [domainA, domainB, domainC],
          x /= y
      ]
    uidA <- randomUser domainA def {BrigI.team = True}
    uidB <- randomUser domainB def {BrigI.team = True}
    uidC <- randomUser domainC def {BrigI.team = True}
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
  withFederatingBackendsAllowDynamic $ \(domainA, domainB, domainC) -> do
    -- A is connected to B and C, but B and C are not connected to each other
    void $ BrigI.createFedConn domainA $ BrigI.FedConn domainB "full_search" Nothing
    void $ BrigI.createFedConn domainB $ BrigI.FedConn domainA "full_search" Nothing
    void $ BrigI.createFedConn domainA $ BrigI.FedConn domainC "full_search" Nothing
    void $ BrigI.createFedConn domainC $ BrigI.FedConn domainA "full_search" Nothing
    uidA <- randomUser domainA def {BrigI.team = True}
    retryT
      $ bindResponse
        (getFederationStatus uidA [domainB, domainC])
      $ \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json %. "status" `shouldMatch` "non-fully-connected"
        resp.json %. "not_connected" `shouldMatchSet` [domainB, domainC]

testFederationStatus :: HasCallStack => App ()
testFederationStatus = do
  uid <- randomUser OwnDomain def {BrigI.team = True}
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
  startDynamicBackends [def, def, def] $ \[domainA, domainB, domainC] -> do
    [u1, u2, u3] <- createUsers [domainA, domainB, domainC]
    connectTwoUsers u1 u2
    connectTwoUsers u1 u3
    bindResponse (postConversation u1 (defProteus {qualifiedUsers = [u2, u3]})) $ \resp -> do
      resp.status `shouldMatchInt` 201

testCreateConversationNonFullyConnected :: HasCallStack => App ()
testCreateConversationNonFullyConnected = do
  withFederatingBackendsAllowDynamic $ \(domainA, domainB, domainC) -> do
    -- A is connected to B and C, but B and C are not connected to each other
    void $ BrigI.createFedConn domainA $ BrigI.FedConn domainB "full_search" Nothing
    void $ BrigI.createFedConn domainB $ BrigI.FedConn domainA "full_search" Nothing
    void $ BrigI.createFedConn domainA $ BrigI.FedConn domainC "full_search" Nothing
    void $ BrigI.createFedConn domainC $ BrigI.FedConn domainA "full_search" Nothing
    liftIO $ threadDelay (2 * 1000 * 1000)

    u1 <- randomUser domainA def
    u2 <- randomUser domainB def
    u3 <- randomUser domainC def
    connectTwoUsers u1 u2
    connectTwoUsers u1 u3

    bindResponse (postConversation u1 (defProteus {qualifiedUsers = [u2, u3]})) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "non_federating_backends" `shouldMatchSet` [domainB, domainC]

testAddMembersFullyConnectedProteus :: HasCallStack => App ()
testAddMembersFullyConnectedProteus = do
  startDynamicBackends [def, def, def] $ \[domainA, domainB, domainC] -> do
    [u1, u2, u3] <- createUsers [domainA, domainB, domainC]
    connectTwoUsers u1 u2
    connectTwoUsers u1 u3
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
    void $ BrigI.createFedConn domainA (BrigI.FedConn domainB "full_search" Nothing)
    void $ BrigI.createFedConn domainB (BrigI.FedConn domainA "full_search" Nothing)
    void $ BrigI.createFedConn domainA (BrigI.FedConn domainC "full_search" Nothing)
    void $ BrigI.createFedConn domainC (BrigI.FedConn domainA "full_search" Nothing)
    liftIO $ threadDelay (2 * 1000 * 1000) -- wait for federation status to be updated

    -- add users
    u1 <- randomUser domainA def
    u2 <- randomUser domainB def
    u3 <- randomUser domainC def
    connectTwoUsers u1 u2
    connectTwoUsers u1 u3

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
  connectTwoUsers alice bob
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
  ([alice, alex, bob, charlie, dylan], domains) <-
    startDynamicBackends [def, def] $ \domains -> do
      own <- make OwnDomain & asString
      other <- make OtherDomain & asString
      users@(alice : others) <- createUsers $ [own, own, other] <> domains
      forM_ others $ connectTwoUsers alice
      pure (users, domains)

  let newConv = defProteus {qualifiedUsers = [alex, bob, charlie, dylan]}
  bindResponse (postConversation alice newConv) $ \resp -> do
    resp.status `shouldMatchInt` 533
    resp.json %. "unreachable_backends" `shouldMatchSet` domains

  convs <- getAllConvs alice >>= asList
  regConvs <- filterM (\c -> (==) <$> (c %. "type" & asInt) <*> pure 0) convs
  regConvs `shouldMatch` ([] :: [Value])

testAddUserWithUnreachableRemoteUsers :: HasCallStack => App ()
testAddUserWithUnreachableRemoteUsers = do
  resourcePool <- asks resourcePool
  own <- make OwnDomain & asString
  other <- make OtherDomain & asString
  runCodensity (acquireResources 1 resourcePool) $ \[cDom] -> do
    ([alex, bobId, bradId, chrisId], conv) <- runCodensity (startDynamicBackend cDom mempty) $ \_ -> do
      [alice, alex, bob, brad, charlie, chris] <-
        createAndConnectUsers [own, own, other, other, cDom.berDomain, cDom.berDomain]

      let newConv = defProteus {qualifiedUsers = [alex, charlie]}
      conv <- postConversation alice newConv >>= getJSON 201
      [bobId, bradId, chrisId] <- forM [bob, brad, chris] (%. "qualified_id")
      pure ([alex, bobId, bradId, chrisId], conv)

    bindResponse (addMembers alex conv def {users = [bobId]}) $ \resp -> do
      resp.status `shouldMatchInt` 533
      resp.jsonBody %. "unreachable_backends" `shouldMatchSet` [cDom.berDomain]

    runCodensity (startDynamicBackend cDom mempty) $ \_ ->
      void $ addMembers alex conv def {users = [bobId]} >>= getBody 200

    -- even though backend C is unreachable, we know B/OtherDomain and C
    -- federate because Bob joined when C was reachable, hence it is OK to add
    -- brad from B to the conversation.
    void $ addMembers alex conv def {users = [bradId]} >>= getBody 200

    -- assert an unreachable user cannot be added
    bindResponse (addMembers alex conv def {users = [chrisId]}) $ \resp -> do
      resp.status `shouldMatchInt` 533
      resp.jsonBody %. "unreachable_backends" `shouldMatchSet` [cDom.berDomain]

testAddUnreachableUserFromFederatingBackend :: HasCallStack => App ()
testAddUnreachableUserFromFederatingBackend = do
  resourcePool <- asks resourcePool
  runCodensity (acquireResources 1 resourcePool) $ \[cDom] -> do
    (alice, chadId, conv) <- runCodensity (startDynamicBackend cDom mempty) $ \_ -> do
      ownDomain <- make OwnDomain & asString
      otherDomain <- make OtherDomain & asString
      [alice, bob, charlie, chad] <-
        createAndConnectUsers [ownDomain, otherDomain, cDom.berDomain, cDom.berDomain]

      conv <- withWebSockets [bob, charlie] $ \wss -> do
        conv <-
          postConversation alice (defProteus {qualifiedUsers = [bob, charlie]})
            >>= getJSON 201
        forM_ wss $ awaitMatch isMemberJoinNotif
        pure conv
      chadId <- chad %. "qualified_id"
      pure (alice, chadId, conv)

    bindResponse (addMembers alice conv def {users = [chadId]}) $ \resp -> do
      resp.status `shouldMatchInt` 533
      resp.jsonBody %. "unreachable_backends" `shouldMatchSet` [cDom.berDomain]

testAddUnreachable :: HasCallStack => App ()
testAddUnreachable = do
  ([alex, charlie], [charlieDomain, dylanDomain], conv) <-
    startDynamicBackends [def, def] $ \domains -> do
      own <- make OwnDomain & asString
      [alice, alex, charlie, dylan] <- createUsers $ [own, own] <> domains
      forM_ [alex, charlie, dylan] $ connectTwoUsers alice

      let newConv = defProteus {qualifiedUsers = [alex, dylan]}
      conv <- postConversation alice newConv >>= getJSON 201
      connectTwoUsers alex charlie
      pure ([alex, charlie], domains, conv)

  charlieId <- charlie %. "qualified_id"
  bindResponse (addMembers alex conv def {users = [charlieId]}) $ \resp -> do
    resp.status `shouldMatchInt` 533
    -- All of the domains that are in the conversation, or will be in the conversation,
    -- need to be reachable so we can check that the graph for those domains is fully connected.
    resp.json %. "unreachable_backends" `shouldMatchSet` [charlieDomain, dylanDomain]

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
    void $ BrigI.createFedConn dynBackend (BrigI.FedConn own "full_search" Nothing)

    alice <- randomUser own def
    bob <- randomUser other def
    charlie <- randomUser dynBackend def
    -- We use retryT here so the dynamic federated connection changes can take
    -- some time to be propagated. Remove after fixing https://wearezeta.atlassian.net/browse/WPB-3797
    mapM_ (retryT . connectTwoUsers alice) [bob, charlie]

    let newConv = defProteus {qualifiedUsers = []}
    conv <- postConversation alice newConv >>= getJSON 201

    bobId <- bob %. "qualified_id"
    charlieId <- charlie %. "qualified_id"
    bindResponse (addMembers alice conv def {users = [bobId, charlieId]}) $ \resp -> do
      resp.status `shouldMatchInt` 409
      resp.json %. "non_federating_backends" `shouldMatchSet` [other, dynBackend]

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
  ([alice, alex], conv) <-
    startDynamicBackends [def] $ \domains -> do
      own <- make OwnDomain & asString
      [alice, alex, charlie] <- createUsers $ [own, own] <> domains
      forM_ [alex, charlie] $ connectTwoUsers alice

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
        mapM_ (connectTwoUsers charlie) [alice, bob]
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
      nameNotif <- awaitNotification charlie client noValue isConvNameChangeNotif
      nameNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      nameNotif %. "payload.0.data.name" `shouldMatch` newConvName
      leaveNotif <- awaitNotification charlie client noValue isConvLeaveNotif
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
      nameNotif <- awaitMatch isConvNameChangeNotif ws
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
      notif <- awaitMatch isReceiptModeUpdateNotif ws
      notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice
      notif %. "payload.0.data.receipt_mode" `shouldMatchInt` 43

testReceiptModeWithRemotesUnreachable :: HasCallStack => App ()
testReceiptModeWithRemotesUnreachable = do
  ownDomain <- asString OwnDomain
  alice <- randomUser ownDomain def
  conv <- startDynamicBackends [mempty] $ \[dynBackend] -> do
    bob <- randomUser dynBackend def
    connectTwoUsers alice bob
    postConversation alice (defProteus {qualifiedUsers = [bob]})
      >>= getJSON 201
  withWebSocket alice $ \ws -> do
    void $ updateReceiptMode alice conv (43 :: Int) >>= getBody 200
    notif <- awaitMatch isReceiptModeUpdateNotif ws
    notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
    notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice
    notif %. "payload.0.data.receipt_mode" `shouldMatchInt` 43

testDeleteLocalMember :: HasCallStack => App ()
testDeleteLocalMember = do
  [alice, alex, bob] <- createUsers [OwnDomain, OwnDomain, OtherDomain]
  connectTwoUsers alice alex
  connectTwoUsers alice bob
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
  [alice, alex, bob] <- createUsers [OwnDomain, OwnDomain, OtherDomain]
  connectTwoUsers alice alex
  connectTwoUsers alice bob
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
  [alice, bob, bart] <- createUsers [OwnDomain, OtherDomain, OtherDomain]
  conv <- startDynamicBackends [mempty] $ \[dynBackend] -> do
    charlie <- randomUser dynBackend def
    connectTwoUsers alice bob
    connectTwoUsers alice bart
    connectTwoUsers alice charlie
    postConversation
      alice
      (defProteus {qualifiedUsers = [bob, bart, charlie]})
      >>= getJSON 201
  void $ withWebSockets [alice, bob] $ \wss -> do
    void $ removeMember alice conv bob >>= getBody 200
    for wss $ \ws -> do
      leaveNotif <- awaitMatch isConvLeaveNotif ws
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
  connectTwoUsers alice bob
  mem <- bob %. "qualified_id"
  void $ addMembers alice conv def {users = [mem]} >>= getBody 200

  void $ withWebSockets [alice, bob] $ \wss -> do
    void $ deleteTeamConversation team conv alice >>= getBody 200
    for wss $ \ws -> do
      notif <- awaitMatch isConvDeleteNotif ws
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
      bob <- randomUser dynBackend.berDomain def
      bobClient <- objId $ bindResponse (addClient bob def) $ getJSON 201
      connectTwoUsers alice bob
      mem <- bob %. "qualified_id"
      void $ addMembers alice conv def {users = [mem]} >>= getBody 200
      pure (bob, bobClient)
    withWebSocket alice $ \ws -> do
      void $ deleteTeamConversation team conv alice >>= getBody 200
      notif <- awaitMatch isConvDeleteNotif ws
      assertNotification notif
    void $ runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
      notif <- awaitNotification bob bobClient noValue isConvDeleteNotif
      assertNotification notif

testDeleteTeamMemberLimitedEventFanout :: HasCallStack => App ()
testDeleteTeamMemberLimitedEventFanout = do
  -- Alex will get removed from the team
  (alice, team, [alex, alison]) <- createTeam OwnDomain 3
  ana <- createTeamMemberWithRole alice team "admin"
  [amy, bob] <- for [OwnDomain, OtherDomain] $ flip randomUser def
  forM_ [amy, bob] $ connectTwoUsers alice
  [aliceId, alexId, amyId, alisonId, anaId, bobId] <- do
    forM [alice, alex, amy, alison, ana, bob] (%. "qualified_id")
  let nc =
        ( defProteus
            { qualifiedUsers =
                [alexId, amyId, alisonId, anaId, bobId],
              team = Just team
            }
        )
  conv <- postConversation alice nc >>= getJSON 201
  memsBefore <- getMembers team aliceId

  -- Only the team admins will get the team-level event about Alex being removed
  -- from the team
  assertSuccess =<< setTeamFeatureStatus OwnDomain team "limitedEventFanout" "enabled"

  withWebSockets [alice, amy, bob, alison, ana] $
    \[wsAlice, wsAmy, wsBob, wsAlison, wsAna] -> do
      void $ deleteTeamMember team alice alex >>= getBody 202

      memsAfter <- getMembers team aliceId
      memsAfter `shouldNotMatch` memsBefore

      assertConvUserDeletedNotif wsAmy alexId
      assertConvUserDeletedNotif wsAlison alexId

      alexUId <- alex %. "id"
      do
        n <- awaitMatch isTeamMemberLeaveNotif wsAlice
        nPayload n %. "data.user" `shouldMatch` alexUId
        assertConvUserDeletedNotif wsAlice alexId
      do
        n <- awaitMatch isTeamMemberLeaveNotif wsAna
        nPayload n %. "data.user" `shouldMatch` alexUId
        assertConvUserDeletedNotif wsAna alexId
      do
        bindResponse (getConversation bob conv) $ \resp -> do
          resp.status `shouldMatchInt` 200
          mems <- resp.json %. "members.others" & asList
          memIds <- forM mems (%. "qualified_id")
          memIds `shouldMatchSet` [aliceId, alisonId, amyId, anaId]
        assertConvUserDeletedNotif wsBob alexId
  where
    getMembers tid usr = bindResponse (getTeamMembers usr tid) $ \resp -> do
      resp.status `shouldMatchInt` 200
      ms <- resp.json %. "members" & asList
      forM ms $ (%. "user")

-- The test relies on the default value for the 'limitedEventFanout' flag, which
-- is disabled by default. The counterpart test
-- 'testDeleteTeamMemberLimitedEventFanout' enables the flag and tests the
-- limited fanout.
testDeleteTeamMemberFullEventFanout :: HasCallStack => App ()
testDeleteTeamMemberFullEventFanout = do
  (alice, team, [alex, alison]) <- createTeam OwnDomain 3
  [amy, bob] <- for [OwnDomain, OtherDomain] $ flip randomUser def
  forM_ [amy, bob] $ connectTwoUsers alice
  [aliceId, alexId, alisonId, amyId, bobId] <-
    forM [alice, alex, alison, amy, bob] (%. "qualified_id")
  let nc = (defProteus {qualifiedUsers = [alexId, alisonId, amyId, bobId], team = Just team})
  conv <- postConversation alice nc >>= getJSON 201
  withWebSockets [alice, alison, amy, bob] $ \[wsAlice, wsAlison, wsAmy, wsBob] -> do
    void $ deleteTeamMember team alice alex >>= getBody 202
    alexUId <- alex %. "id"
    do
      n <- awaitMatch isTeamMemberLeaveNotif wsAlice
      nPayload n %. "data.user" `shouldMatch` alexUId
    do
      t <- awaitMatch isTeamMemberLeaveNotif wsAlison
      nPayload t %. "data.user" `shouldMatch` alexUId
      assertConvUserDeletedNotif wsAlison alexId

    assertConvUserDeletedNotif wsAmy alexId

    do
      bindResponse (getConversation bob conv) $ \resp -> do
        resp.status `shouldMatchInt` 200
        mems <- resp.json %. "members.others" & asList
        memIds <- forM mems (%. "qualified_id")
        memIds `shouldMatchSet` [aliceId, alisonId, amyId]
      assertConvUserDeletedNotif wsBob alexId

testLeaveConversationSuccess :: HasCallStack => App ()
testLeaveConversationSuccess = do
  [alice, bob, chad, dee] <- createUsers [OwnDomain, OwnDomain, OtherDomain, OtherDomain]
  [aClient, bClient] <- forM [alice, bob] $ \user ->
    objId $ bindResponse (addClient user def) $ getJSON 201
  startDynamicBackends [def] $ \[dynDomain] -> do
    eve <- randomUser dynDomain def
    eClient <- objId $ bindResponse (addClient eve def) $ getJSON 201
    forM_ [bob, chad, dee, eve] $ connectTwoUsers alice
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
  startDynamicBackends [def] $ \[dynDomain] -> do
    [ownDomain, otherDomain] <- forM [OwnDomain, OtherDomain] asString
    [alice, alex, bob, bart, chad] <- createUsers [ownDomain, ownDomain, otherDomain, otherDomain, dynDomain]
    forM_ [alex, bob, bart, chad] $ connectTwoUsers alice
    bobId <- bob %. "qualified_id"
    ooConvId <-
      getOne2OneConversation alice bobId Established >>= (%. "qualified_id")

    mainConvBefore <-
      postConversation alice (defProteus {qualifiedUsers = [alex, bob, bart, chad]})
        >>= getJSON 201

    void $ withWebSocket alex $ \ws -> do
      void $ deleteUser bob >>= getBody 200
      n <- awaitMatch isConvLeaveNotif ws
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
  [alice, bob, charlie] <- createUsers [OwnDomain, OtherDomain, OtherDomain]
  connectTwoUsers alice bob
  connectTwoUsers alice charlie
  conv <-
    postConversation alice (defProteus {qualifiedUsers = [bob, charlie]})
      >>= getJSON 201
  void $ updateRole alice bob "wire_admin" (conv %. "qualified_id") >>= getBody 200
  void $ withWebSockets [alice, bob, charlie] $ \wss -> do
    void $ updateReceiptMode bob conv (41 :: Int) >>= getBody 200
    for_ wss $ \ws -> awaitMatch isReceiptModeUpdateNotif ws

testGuestCreatesConversation :: HasCallStack => App ()
testGuestCreatesConversation = do
  alice <- randomUser OwnDomain def {BrigI.activate = False}
  bindResponse (postConversation alice defProteus) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-denied"

testGuestLinksSuccess :: HasCallStack => App ()
testGuestLinksSuccess = do
  (user, _, tm : _) <- createTeam OwnDomain 2
  conv <- postConversation user (allowGuests defProteus) >>= getJSON 201
  (k, v) <- bindResponse (postConversationCode user conv Nothing Nothing) $ \resp -> do
    res <- getJSON 201 resp
    k <- res %. "data.key" & asString
    v <- res %. "data.code" & asString
    pure (k, v)
  bindResponse (getJoinCodeConv tm k v) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "id" `shouldMatch` objId conv

testGuestLinksExpired :: HasCallStack => App ()
testGuestLinksExpired = do
  withModifiedBackend
    def {galleyCfg = setField "settings.guestLinkTTLSeconds" (1 :: Int)}
    $ \domain -> do
      (user, _, tm : _) <- createTeam domain 2
      conv <- postConversation user (allowGuests defProteus) >>= getJSON 201
      (k, v) <- bindResponse (postConversationCode user conv Nothing Nothing) $ \resp -> do
        res <- getJSON 201 resp
        (,) <$> asString (res %. "data.key") <*> asString (res %. "data.code")
      -- let's wait a little longer than 1 second for the guest link to expire
      liftIO $ threadDelay (1_100_000)
      bindResponse (getJoinCodeConv tm k v) $ \resp -> do
        resp.status `shouldMatchInt` 404

testConversationWithFedV0 :: HasCallStack => App ()
testConversationWithFedV0 = do
  alice <- randomUser OwnDomain def
  bob <- randomUser FedV0Domain def
  withAPIVersion 4 $ connectTwoUsers alice bob

  conv <-
    postConversation alice (defProteus {qualifiedUsers = [bob]})
      >>= getJSON 201

  withWebSocket bob $ \ws -> do
    void $ changeConversationName alice conv "foobar" >>= getJSON 200
    void $ awaitMatch isConvNameChangeNotif ws

testConversationWithoutFederation :: HasCallStack => App ()
testConversationWithoutFederation = withModifiedBackend
  (def {galleyCfg = removeField "federator" >=> removeField "rabbitmq"})
  $ \domain -> do
    [alice, bob] <- createAndConnectUsers [domain, domain]
    void $ postConversation alice (defProteus {qualifiedUsers = [bob]}) >>= getJSON 201
