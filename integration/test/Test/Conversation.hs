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
import API.GalleyInternal hiding (getConversation)
import qualified API.GalleyInternal as I
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Stack
import MLS.Util
import Notifications
import SetupHelpers hiding (deleteUser)
import Testlib.One2One (generateRemoteAndConvIdWithDomain)
import Testlib.Prelude
import Testlib.ResourcePool
import Testlib.VersionedFed
import Text.Regex.TDFA ((=~))
import UnliftIO

testFederatedConversation :: (HasCallStack) => App ()
testFederatedConversation = do
  -- This test was created to verify that the false positive log message:
  -- "Attempt to send notification about conversation update to users not in the conversation"
  -- does not happen when a user is added to a conversation that is federated.
  -- Unfortunately, that can only be manually verified by looking at the logs.
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  conv <- postConversation alice defProteus >>= getJSON 201

  withWebSocket bob $ \bobWs -> do
    addMembers alice conv def {users = [bob]} >>= assertSuccess
    void $ awaitMatch isMemberJoinNotif bobWs

  checkConvMembers conv alice [bob]
  retryT $ checkConvMembers conv bob [alice]
  where
    checkConvMembers :: (HasCallStack, MakesValue user) => Value -> user -> [Value] -> App ()
    checkConvMembers conv self others =
      bindResponse (getConversation self conv) $ \resp -> do
        resp.status `shouldMatchInt` 200
        mems <- resp.json %. "members.others" & asList
        for mems (%. "qualified_id") `shouldMatchSet` (for others (%. "qualified_id"))

testDynamicBackendsFullyConnectedWhenAllowAll :: (HasCallStack) => App ()
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

testDynamicBackendsNotFederating :: (HasCallStack) => App ()
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

testDynamicBackendsFullyConnectedWhenAllowDynamic :: (HasCallStack) => App ()
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

testDynamicBackendsNotFullyConnected :: (HasCallStack) => App ()
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

testFederationStatus :: (HasCallStack) => StaticDomain -> App ()
testFederationStatus domain = do
  uid <- randomUser OwnDomain def {BrigI.team = True}
  federatingRemoteDomain <- asString domain
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

testCreateConversationFullyConnected :: (HasCallStack) => App ()
testCreateConversationFullyConnected = do
  startDynamicBackends [def, def, def] $ \[domainA, domainB, domainC] -> do
    [u1, u2, u3] <- createUsers [domainA, domainB, domainC]
    connectTwoUsers u1 u2
    connectTwoUsers u1 u3
    bindResponse (postConversation u1 (defProteus {qualifiedUsers = [u2, u3]})) $ \resp -> do
      resp.status `shouldMatchInt` 201

testCreateConversationNonFullyConnected :: (HasCallStack) => App ()
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

testAddMembersFullyConnectedProteus :: (HasCallStack) => App ()
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

testAddMembersNonFullyConnectedProteus :: (HasCallStack) => App ()
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

testAddMember :: (HasCallStack) => App ()
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

testAddMemberV1 :: (HasCallStack) => Domain -> App ()
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

testConvWithUnreachableRemoteUsers :: (HasCallStack) => StaticDomain -> App ()
testConvWithUnreachableRemoteUsers domain = do
  ([alice, alex, bob, charlie, dylan], domains) <-
    startDynamicBackends [def, def] $ \domains -> do
      own <- make OwnDomain & asString
      other <- make domain & asString
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

testAddUserWithUnreachableRemoteUsers :: (HasCallStack) => StaticDomain -> App ()
testAddUserWithUnreachableRemoteUsers domain = do
  resourcePool <- asks resourcePool
  own <- make OwnDomain & asString
  other <- make domain & asString
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

    -- even though backend C is unreachable, we know B/domain and C
    -- federate because Bob joined when C was reachable, hence it is OK to add
    -- brad from B to the conversation.
    void $ addMembers alex conv def {users = [bradId]} >>= getBody 200

    -- assert an unreachable user cannot be added
    bindResponse (addMembers alex conv def {users = [chrisId]}) $ \resp -> do
      resp.status `shouldMatchInt` 533
      resp.jsonBody %. "unreachable_backends" `shouldMatchSet` [cDom.berDomain]

testAddUnreachableUserFromFederatingBackend :: (HasCallStack) => StaticDomain -> App ()
testAddUnreachableUserFromFederatingBackend domain = do
  resourcePool <- asks resourcePool
  runCodensity (acquireResources 1 resourcePool) $ \[cDom] -> do
    (alice, chadId, conv) <- runCodensity (startDynamicBackend cDom mempty) $ \_ -> do
      ownDomain <- make OwnDomain & asString
      otherDomain <- make domain & asString
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

testAddUnreachable :: (HasCallStack) => App ()
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

testGetOneOnOneConvInStatusSentFromRemote :: (HasCallStack) => StaticDomain -> App ()
testGetOneOnOneConvInStatusSentFromRemote domain = do
  d1User <- randomUser OwnDomain def
  let shouldBeLocal = True
  (d2Usr, d2ConvId) <- generateRemoteAndConvIdWithDomain domain (not shouldBeLocal) d1User
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

testAddingUserNonFullyConnectedFederation :: (HasCallStack) => StaticDomain -> App ()
testAddingUserNonFullyConnectedFederation domain = do
  let overrides =
        def
          { brigCfg =
              setField "optSettings.setFederationStrategy" "allowDynamic"
          }
  startDynamicBackends [overrides] $ \[dynBackend] -> do
    own <- asString OwnDomain
    other <- asString domain

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

testMultiIngressGuestLinks :: (HasCallStack) => App ()
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

testAddUserWhenOtherBackendOffline :: (HasCallStack) => App ()
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

testSynchroniseUserRemovalNotification :: (HasCallStack) => StaticDomain -> App ()
testSynchroniseUserRemovalNotification domain = do
  resourcePool <- asks resourcePool
  ownDomain <- make OwnDomain
  otherDomain <- make domain
  [alice, bob] <- createAndConnectUsers [ownDomain, otherDomain]
  runCodensity (acquireResources 1 resourcePool) $ \[dynBackend] -> do
    (conv, charlie) <-
      runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
        charlie <- randomUser dynBackend.berDomain def
        mapM_ (connectTwoUsers charlie) [alice, bob]
        conv <-
          postConversation alice (defProteus {qualifiedUsers = [bob, charlie]})
            >>= getJSON 201
        pure (conv, charlie)

    let newConvName = "The new conversation name"
    bindResponse (changeConversationName alice conv newConvName) $ \resp ->
      resp.status `shouldMatchInt` 200
    bindResponse (removeMember alice conv charlie) $ \resp ->
      resp.status `shouldMatchInt` 200
    runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
      nameNotif <- awaitNotification charlie noValue isConvNameChangeNotif
      nameNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
      nameNotif %. "payload.0.data.name" `shouldMatch` newConvName
      leaveNotif <- awaitNotification charlie noValue isConvLeaveNotif
      leaveNotif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv

testConvRenaming :: (HasCallStack) => App ()
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

testNewConversationReceiptMode :: (HasCallStack) => ConversationProtocol -> App ()
testNewConversationReceiptMode proto = do
  alice <- randomUser OwnDomain def
  conv <- postConversation alice (defConv proto) {receiptMode = Just 11} >>= getJSON 201
  let expectedReceiptMode = case proto of
        ConversationProtocolProteus -> 11
        ConversationProtocolMLS -> 0
  conv %. "receipt_mode" `shouldMatchInt` expectedReceiptMode

testConversationReceiptModeUpdate :: (HasCallStack) => ConversationProtocol -> App ()
testConversationReceiptModeUpdate proto = do
  alice <- randomUser OwnDomain def
  conv <- postConversation alice (defConv proto) {receiptMode = Just 11} >>= getJSON 201
  receiptMode <- bindResponse (updateReceiptMode alice conv (12 :: Int)) $ \resp -> case proto of
    ConversationProtocolProteus -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "data.receipt_mode" `shouldMatchInt` 12
      pure 12
    ConversationProtocolMLS -> do
      resp.status `shouldMatchInt` 403
      resp.json %. "label" `shouldMatch` "mls-receipts-not-allowed"
      pure 0

  bindResponse (getConversation alice conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "receipt_mode" `shouldMatchInt` receiptMode

testReceiptModeWithRemotesOk :: (HasCallStack) => App ()
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

testReceiptModeWithRemotesUnreachable :: (HasCallStack) => App ()
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

testDeleteLocalMember :: (HasCallStack) => App ()
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

testDeleteRemoteMember :: (HasCallStack) => App ()
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

testDeleteRemoteMemberRemoteUnreachable :: (HasCallStack) => App ()
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

testDeleteTeamConversationWithRemoteMembers :: (HasCallStack) => App ()
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

testDeleteTeamConversationWithUnreachableRemoteMembers :: (HasCallStack) => App ()
testDeleteTeamConversationWithUnreachableRemoteMembers = do
  resourcePool <- asks resourcePool
  (alice, team, _) <- createTeam OwnDomain 1
  conv <- postConversation alice (defProteus {team = Just team}) >>= getJSON 201

  let assertNotification :: (HasCallStack, MakesValue n) => n -> App ()
      assertNotification notif = do
        notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
        notif %. "payload.0.qualified_from" `shouldMatch` objQidObject alice

  runCodensity (acquireResources 1 resourcePool) $ \[dynBackend] -> do
    bob <- runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
      bob <- randomUser dynBackend.berDomain def
      connectTwoUsers alice bob
      mem <- bob %. "qualified_id"
      void $ addMembers alice conv def {users = [mem]} >>= getBody 200
      pure bob
    withWebSocket alice $ \ws -> do
      void $ deleteTeamConversation team conv alice >>= getBody 200
      notif <- awaitMatch isConvDeleteNotif ws
      assertNotification notif
    void $ runCodensity (startDynamicBackend dynBackend mempty) $ \_ -> do
      notif <- awaitNotification bob noValue isConvDeleteNotif
      assertNotification notif

testDeleteTeamMemberLimitedEventFanout :: (HasCallStack) => App ()
testDeleteTeamMemberLimitedEventFanout = do
  -- Alex will get removed from the team
  (alice, team, [alex, alison]) <- createTeam OwnDomain 3
  ana <- createTeamMember alice def {role = "admin"}
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

  withWebSockets [alice, amy, bob, alison, ana]
    $ \[wsAlice, wsAmy, wsBob, wsAlison, wsAna] -> do
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
testDeleteTeamMemberFullEventFanout :: (HasCallStack) => App ()
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

testLeaveConversationSuccess :: (HasCallStack) => App ()
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

testOnUserDeletedConversations :: (HasCallStack) => App ()
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

testUpdateConversationByRemoteAdmin :: (HasCallStack) => App ()
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

testGuestCreatesConversation :: (HasCallStack) => App ()
testGuestCreatesConversation = do
  alice <- randomUser OwnDomain def {BrigI.activate = False}
  bindResponse (postConversation alice defProteus) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-denied"

testGuestLinksSuccess :: (HasCallStack) => App ()
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
    resp.json %. "id" `shouldMatch` (objQidObject conv & objId)

testGuestLinksExpired :: (HasCallStack) => App ()
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

testConversationWithLegacyFed :: (HasCallStack) => AnyFedDomain -> App ()
testConversationWithLegacyFed domain = do
  alice <- randomUser OwnDomain def
  bob <- randomUser domain def
  withAPIVersion 4 $ connectTwoUsers alice bob

  conv <-
    postConversation alice (defProteus {qualifiedUsers = [bob]})
      >>= getJSON 201

  withWebSocket bob $ \ws -> do
    void $ changeConversationName alice conv "foobar" >>= getJSON 200
    void $ awaitMatch isConvNameChangeNotif ws

testConversationWithoutFederation :: (HasCallStack) => App ()
testConversationWithoutFederation = withModifiedBackend
  (def {galleyCfg = removeField "federator" >=> removeField "rabbitmq"})
  $ \domain -> do
    [alice, bob] <- createAndConnectUsers [domain, domain]
    void $ postConversation alice (defProteus {qualifiedUsers = [bob]}) >>= getJSON 201

testPostConvWithUnreachableRemoteUsers :: App ()
testPostConvWithUnreachableRemoteUsers = do
  [alice, alex] <- createAndConnectUsers [OwnDomain, OtherDomain]
  resourcePool <- asks resourcePool
  runCodensity (acquireResources 2 resourcePool) $ \[unreachableBackend, reachableBackend] -> do
    runCodensity (startDynamicBackend reachableBackend mempty) $ \_ -> do
      unreachableUsers <- runCodensity (startDynamicBackend unreachableBackend mempty) $ \_ -> do
        let downDomain = unreachableBackend.berDomain
        ownDomain <- asString OwnDomain
        otherDomain <- asString OtherDomain
        void $ BrigI.createFedConn downDomain (BrigI.FedConn ownDomain "full_search" Nothing)
        void $ BrigI.createFedConn downDomain (BrigI.FedConn otherDomain "full_search" Nothing)
        users <- replicateM 3 (randomUser downDomain def)
        for_ users $ \user -> do
          connectUsers [alice, user]
          connectUsers [alex, user]
        -- creating the conv here would work.
        pure users

      reachableUsers <- replicateM 2 (randomUser reachableBackend.berDomain def)
      for_ reachableUsers $ \user -> do
        connectUsers [alice, user]
        connectUsers [alex, user]

      withWebSockets [alice, alex] $ \[wssAlice, wssAlex] -> do
        -- unreachableBackend is still allocated, but the backend is down.  creating the conv here doesn't work.
        let payload = defProteus {name = Just "some chat", qualifiedUsers = [alex] <> reachableUsers <> unreachableUsers}
        postConversation alice payload >>= assertStatus 533

        convs <- getAllConvs alice
        for_ convs $ \conv -> conv %. "type" `shouldNotMatchInt` 0
        assertNoEvent 2 wssAlice
        assertNoEvent 2 wssAlex

testNoFederationWithProteus :: (HasCallStack) => App ()
testNoFederationWithProteus = do
  withModifiedBackend
    ( def
        { galleyCfg = \conf ->
            conf & setField "settings.federationProtocols" ["mls"]
        }
    )
    $ \domain -> do
      charlieDomain <- asString $ make OwnDomain
      [alice, alex, arnold, bob] <- createAndConnectUsers [domain, domain, domain, charlieDomain]

      do
        conv <- postConversation alice defProteus {qualifiedUsers = [alex]} >>= getJSON 201
        bindResponse (addMembers alice conv def {users = [bob]}) $ \resp -> do
          resp.status `shouldMatchInt` 409
          resp.json %. "label" `shouldMatch` "federation-disabled-for-protocol"
        void $ addMembers alice conv def {users = [arnold]} >>= getJSON 200

      bindResponse (postConversation alice defProteus {qualifiedUsers = [bob]}) $ \resp -> do
        resp.status `shouldMatchInt` 409
        resp.json %. "label" `shouldMatch` "federation-disabled-for-protocol"

      void $ postConversation bob defProteus {qualifiedUsers = [alice]} >>= getJSON 201

testGetConversationInternal :: (HasCallStack) => App ()
testGetConversationInternal = do
  (owner, tid, mems) <- createTeam OwnDomain 2
  conv <- postConversation owner (defProteus {team = Just tid, qualifiedUsers = mems}) >>= getJSON 201
  I.getConversation conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "qualified_id" `shouldMatch` objQidObject conv
    members <- resp.json %. "members.others" & asList
    memberIds <- for members (%. "qualified_id")
    memberIds `shouldMatchSet` (for (owner : mems) (%. "qualified_id"))
    lookupField resp.json "members.self" `shouldMatch` (Nothing @Value)

testGetSelfMember :: (HasCallStack) => App ()
testGetSelfMember = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  conv <-
    postConversation alice (defProteus {qualifiedUsers = [bob], newUsersRole = "wire_member"})
      >>= getJSON 201
  bindResponse (getSelfMember alice conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "conversation_role" `shouldMatch` "wire_admin"
    resp.json %. "hidden" `shouldMatch` False
    resp.json %. "hidden_ref" `shouldMatch` Null
    resp.json %. "otr_archived" `shouldMatch` False
    resp.json %. "otr_archived_ref" `shouldMatch` Null
    resp.json %. "otr_muted_ref" `shouldMatch` Null
    resp.json %. "otr_muted_status" `shouldMatch` Null
    resp.json %. "qualified_id" `shouldMatch` (alice %. "qualified_id")
    resp.json %. "service" `shouldMatch` Null
    resp.json %. "status" `shouldMatchInt` 0
    resp.json %. "status_ref" `shouldMatch` "0.0"

  bindResponse (getSelfMember bob conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "conversation_role" `shouldMatch` "wire_member"
    resp.json %. "hidden" `shouldMatch` False
    resp.json %. "hidden_ref" `shouldMatch` Null
    resp.json %. "otr_archived" `shouldMatch` False
    resp.json %. "otr_archived_ref" `shouldMatch` Null
    resp.json %. "otr_muted_ref" `shouldMatch` Null
    resp.json %. "otr_muted_status" `shouldMatch` Null
    resp.json %. "qualified_id" `shouldMatch` (bob %. "qualified_id")
    resp.json %. "service" `shouldMatch` Null
    resp.json %. "status" `shouldMatchInt` 0
    resp.json %. "status_ref" `shouldMatch` "0.0"

-- | The migration has these phases.
-- 1. Write to cassandra (before any migration activity)
-- 2. Galley is prepared for migrations (new things created in PG, old things are in Cassandra)
-- 3. Backgound worker starts migration
-- 4. Background worker finishes migration, galley is still configured to think migration is on going
-- 5. Background worker is configured to not do anything, galley is configured to only use PG
--
-- The comments and variable names call these phases by number i.e. Phase1, Phase2, and so on.
--
-- The tests are from the perspective of mel, a user on the dynamic backend,
-- called backendM (migraing backend). There are also users called mark and mia
-- on this backend.
--
-- TODO:
-- Also create convs and send messages in all phases
testMigrationToPostgresMLS :: App ()
testMigrationToPostgresMLS = do
  resourcePool <- asks (.resourcePool)
  (alice, aliceTid, _) <- createTeam OwnDomain 1
  (bob, bobTid, _) <- createTeam OtherDomain 1
  [aliceC, bobC] <- traverse (createMLSClient def) [alice, bob]

  let phase1Overrides =
        def
          { galleyCfg = setField "postgresMigration.conversation" "cassandra",
            backgroundWorkerCfg = setField "migrateConversations" False
          }
      phase2Overrides =
        def
          { galleyCfg = setField "postgresMigration.conversation" "migration-to-postgresql",
            backgroundWorkerCfg = setField "migrateConversations" False
          }
      phase3Overrides =
        def
          { galleyCfg = setField "postgresMigration.conversation" "migration-to-postgresql",
            backgroundWorkerCfg = setField "migrateConversations" True
          }
      phase4Overrides =
        def
          { galleyCfg = setField "postgresMigration.conversation" "migration-to-postgresql",
            backgroundWorkerCfg = setField "migrateConversations" False
          }
      phase5Overrides =
        def
          { galleyCfg = setField "postgresMigration.conversation" "postgresql",
            backgroundWorkerCfg = setField "migrateConversations" False
          }
      phaseOverrides =
        IntMap.fromList
          [ (1, phase1Overrides),
            (2, phase2Overrides),
            (3, phase3Overrides),
            (4, phase4Overrides),
            (5, phase5Overrides)
          ]
  runCodensity (acquireResources 1 resourcePool) $ \[migratingBackend] -> do
    let domainM = migratingBackend.berDomain
    (mel, melC, mark, markC, mia, miaC, miaTid, domainAConvs, domainBConvs, domainMConvs, otherMelConvs) <- runCodensity (startDynamicBackend migratingBackend phase1Overrides) $ \_ -> do
      [mel, mark] <- createUsers [domainM, domainM]
      (mia, miaTid, _) <- createTeam domainM 1
      [melC, markC, miaC] <- traverse (createMLSClient def) [mel, mark, mia]
      connectUsers [alice, bob, mel, mark, mia]
      otherMelConvs <- getAllConvIds mel 100

      domainAConvs <- createTestConvs aliceC aliceTid melC markC []
      domainBConvs <- createTestConvs bobC bobTid melC markC []
      domainMConvs <- createTestConvs miaC miaTid melC markC []
      pure (mel, melC, mark, markC, mia, miaC, miaTid, domainAConvs, domainBConvs, domainMConvs, otherMelConvs)

    addUsersToFailureContext [("alice", alice), ("bob", bob), ("mel", mel), ("mark", mark), ("mia", mia)]
      $ addJSONToFailureContext "convIds" (domainAConvs <> domainBConvs <> domainMConvs)
      $ addJSONToFailureContext "otherMelConvs" otherMelConvs
      $ do
        let runPhase :: (HasCallStack) => Int -> App ()
            runPhase phase = do
              putStrLn $ "----------> Start phase: " <> show phase
              runCodensity (startDynamicBackend migratingBackend (phaseOverrides IntMap.! phase)) $ \_ -> do
                runPhaseOperations phase aliceC aliceTid domainAConvs melC markC
                runPhaseOperations phase bobC bobTid domainBConvs melC markC
                runPhaseOperations phase miaC miaTid domainMConvs melC markC
                actualConvs <- getAllConvIds mel n
                let expectedConvsFrom dom =
                      dom.unmodifiedConvs
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.kickMelConvs (IntSet.fromList [(phase + 1) .. 5])))
                        <> concat (IntMap.elems dom.kickMarkConvs)
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.delConvs (IntSet.fromList [(phase + 1) .. 5])))
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.addMelConvs (IntSet.fromList [(phase + 1) .. 5])))
                    expectedConvs =
                      expectedConvsFrom domainAConvs
                        <> expectedConvsFrom domainBConvs
                        <> expectedConvsFrom domainMConvs

                actualConvs `shouldMatchSet` ((convIdToQidObject <$> expectedConvs) <> otherMelConvs)

                when (phase == 3) $ waitForMigration domainM
        runPhase 1
        runPhase 2
        runPhase 3
        runPhase 4
        runPhase 5
  where
    n = 1
    -- Creates n convs of these types:
    -- 1. Convs that will exist unmodified during the test
    -- 2. Convs that will kick mel in each phase
    -- 3. Convs that will kick mark in each phase
    -- 4. Convs that will be deleted in each phase
    createTestConvs :: (HasCallStack) => ClientIdentity -> String -> ClientIdentity -> ClientIdentity -> [ClientIdentity] -> App TestConvList
    createTestConvs creatorC tid melC markC othersC = do
      unmodifiedConvs <- replicateM n $ do
        createTestConv creatorC tid (melC : markC : othersC)

      kickMelConvs <- forPhase $ createTestConv creatorC tid (melC : othersC)
      kickMarkConvs <- forPhase $ createTestConv creatorC tid (melC : markC : othersC)
      delConvs <- forPhase $ createTestConv creatorC tid (melC : markC : othersC)
      addMelConvs <- forPhase $ createTestConv creatorC tid othersC
      pure $ TestConvList {..}

    createTestConv :: (HasCallStack) => ClientIdentity -> String -> [ClientIdentity] -> App ConvId
    createTestConv creatorC tid membersC = do
      conv <- createNewGroupWith def creatorC defMLS {team = Just tid}
      traverse_ (uploadNewKeyPackage def) membersC
      void $ createAddCommit creatorC conv ((.qualifiedUserId) <$> membersC) >>= sendAndConsumeCommitBundle
      pure conv

    forPhase :: App a -> App (IntMap [a])
    forPhase action =
      fmap IntMap.fromList . for [1 .. 5] $ \phase -> do
        convs <- replicateM n $ action
        pure (phase, convs)

    runPhaseOperations :: (HasCallStack) => Int -> ClientIdentity -> String -> TestConvList -> ClientIdentity -> ClientIdentity -> App ()
    runPhaseOperations phase convAdmin tid TestConvList {..} melC markC = do
      for_ (IntMap.findWithDefault [] phase kickMelConvs) $ \convId -> do
        mp <- createRemoveCommit convAdmin convId [melC]
        void $ postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 201

      for_ (IntMap.findWithDefault [] phase kickMarkConvs) $ \convId -> do
        mp <- createRemoveCommit convAdmin convId [markC]
        void $ postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 201

      for_ (IntMap.findWithDefault [] phase delConvs) $ \convId -> do
        deleteTeamConversation tid convId convAdmin >>= assertSuccess
        getConversation convAdmin convId `bindResponse` \resp ->
          resp.status `shouldMatchInt` 404

      for_ (IntMap.findWithDefault [] phase addMelConvs) $ \convId -> do
        void $ uploadNewKeyPackage def melC
        void $ createAddCommit convAdmin convId [melC.qualifiedUserId] >>= sendAndConsumeCommitBundle

    waitForMigration :: (HasCallStack) => String -> App ()
    waitForMigration domainM = do
      metrics <-
        getMetrics domainM BackgroundWorker `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          pure $ Text.decodeUtf8 resp.body
      let (_, _, _, convFinishedMatches) :: (Text, Text, Text, [Text]) = (metrics =~ Text.pack "^wire_local_convs_migration_finished\\ ([0-9]+\\.[0-9]+)$")
      let (_, _, _, userFinishedMatches) :: (Text, Text, Text, [Text]) = (metrics =~ Text.pack "^wire_user_remote_convs_migration_finished\\ ([0-9]+\\.[0-9]+)$")
      when (convFinishedMatches /= [Text.pack "1.0"] || userFinishedMatches /= [Text.pack "1.0"]) $ do
        liftIO $ threadDelay 100_000
        waitForMigration domainM

-- | The migration has these phases.
-- 1. Write to cassandra (before any migration activity)
-- 2. Galley is prepared for migrations (new things created in PG, old things are in Cassandra)
-- 3. Backgound worker starts migration
-- 4. Background worker finishes migration, galley is still configured to think migration is on going
-- 5. Background worker is configured to not do anything, galley is configured to only use PG
--
-- The comments and variable names call these phases by number i.e. Phase1, Phase2, and so on.
--
-- The tests are from the perspective of mel, a user on the dynamic backend,
-- called backendM (migraing backend). There are also users called mark and mia
-- on this backend.
--
-- TODO:
-- Also create convs and send messages in all phases
testMigrationToPostgresJustProteus :: App ()
testMigrationToPostgresJustProteus = do
  resourcePool <- asks (.resourcePool)
  (alice, aliceTid, _) <- createTeam OwnDomain 1
  (bob, bobTid, _) <- createTeam OtherDomain 1

  let phase1Overrides =
        def
          { galleyCfg = setField "postgresMigration.conversation" "cassandra",
            backgroundWorkerCfg = setField "migrateConversations" False
          }
      phase2Overrides =
        def
          { galleyCfg = setField "postgresMigration.conversation" "migration-to-postgresql",
            backgroundWorkerCfg = setField "migrateConversations" False
          }
      phase3Overrides =
        def
          { galleyCfg = setField "postgresMigration.conversation" "migration-to-postgresql",
            backgroundWorkerCfg = setField "migrateConversations" True
          }
      phase4Overrides =
        def
          { galleyCfg = setField "postgresMigration.conversation" "migration-to-postgresql",
            backgroundWorkerCfg = setField "migrateConversations" False
          }
      phase5Overrides =
        def
          { galleyCfg = setField "postgresMigration.conversation" "postgresql",
            backgroundWorkerCfg = setField "migrateConversations" False
          }
      phaseOverrides =
        IntMap.fromList
          [ (1, phase1Overrides),
            (2, phase2Overrides),
            (3, phase3Overrides),
            (4, phase4Overrides),
            (5, phase5Overrides)
          ]
  runCodensity (acquireResources 1 resourcePool) $ \[migratingBackend] -> do
    let domainM = migratingBackend.berDomain
    (mel, _melC, mark, _markC, mia, _miaC, miaTid, domainAConvs, domainBConvs, domainMConvs, otherMelConvs) <- runCodensity (startDynamicBackend migratingBackend phase1Overrides) $ \_ -> do
      [mel, mark] <- createUsers [domainM, domainM]
      (mia, miaTid, _) <- createTeam domainM 1
      [melC, markC, miaC] <- traverse (createMLSClient def) [mel, mark, mia]
      connectUsers [alice, bob, mel, mark, mia]
      otherMelConvs <- getAllConvIds mel 100

      domainAConvs <- createTestConvs alice aliceTid mel mark []
      domainBConvs <- createTestConvs bob bobTid mel mark []
      domainMConvs <- createTestConvs mia miaTid mel mark []
      pure (mel, melC, mark, markC, mia, miaC, miaTid, domainAConvs, domainBConvs, domainMConvs, otherMelConvs)

    newConvsRef <- newIORef []
    addUsersToFailureContext [("alice", alice), ("bob", bob), ("mel", mel), ("mark", mark), ("mia", mia)]
      $ addJSONToFailureContext "convIds" (domainAConvs <> domainBConvs <> domainMConvs)
      $ addJSONToFailureContext "otherMelConvs" otherMelConvs
      $ do
        let runPhase :: (HasCallStack) => Int -> App ()
            runPhase phase = do
              putStrLn $ "----------> Start phase: " <> show phase
              runCodensity (startDynamicBackend migratingBackend (phaseOverrides IntMap.! phase)) $ \_ -> do
                newDomainAConvs <- runPhaseOperations phase alice aliceTid domainAConvs mel mark
                newDomainBConvs <- runPhaseOperations phase bob bobTid domainBConvs mel mark
                newDomainCConvs <- runPhaseOperations phase mia miaTid domainMConvs mel mark
                let newConvs = newDomainAConvs <> newDomainBConvs <> newDomainCConvs
                modifyIORef newConvsRef (newConvs <>)
                allNewConvs <- readIORef newConvsRef
                actualConvs <- getAllConvIds mel n
                let expectedConvsFrom dom =
                      dom.unmodifiedConvs
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.kickMelConvs (IntSet.fromList [(phase + 1) .. 5])))
                        <> concat (IntMap.elems dom.kickMarkConvs)
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.delConvs (IntSet.fromList [(phase + 1) .. 5])))
                        <> concat (IntMap.elems (IntMap.restrictKeys dom.addMelConvs (IntSet.fromList [1 .. phase])))
                    expectedConvs =
                      expectedConvsFrom domainAConvs
                        <> expectedConvsFrom domainBConvs
                        <> expectedConvsFrom domainMConvs
                        <> allNewConvs

                actualConvs `shouldMatchSet` ((convIdToQidObject <$> expectedConvs) <> otherMelConvs)

                when (phase == 3) $ waitForMigration domainM
        runPhase 1
        runPhase 2
        runPhase 3
        runPhase 4
        runPhase 5
  where
    n = 10
    -- Creates n convs of these types:
    -- 1. Convs that will exist unmodified during the test
    -- 2. Convs that will kick mel in each phase
    -- 3. Convs that will kick mark in each phase
    -- 4. Convs that will be deleted in each phase
    -- 5. Convs that will add mel in each phase
    createTestConvs :: (HasCallStack) => Value -> String -> Value -> Value -> [Value] -> App TestConvList
    createTestConvs creatorC tid mel mark others = do
      unmodifiedConvs <- replicateConcurrently n $ do
        createTestConv creatorC tid (mel : mark : others)

      kickMelConvs <- forPhase $ createTestConv creatorC tid (mel : others)
      kickMarkConvs <- forPhase $ createTestConv creatorC tid (mel : mark : others)
      delConvs <- forPhase $ createTestConv creatorC tid (mel : mark : others)
      addMelConvs <- forPhase $ createTestConv creatorC tid others
      pure $ TestConvList {..}

    createTestConv :: (HasCallStack) => Value -> String -> [Value] -> App ConvId
    createTestConv creator tid members = do
      postConversation creator defProteus {team = Just tid, qualifiedUsers = members}
        >>= getJSON 201
        >>= objConvId

    forPhase :: App a -> App (IntMap [a])
    forPhase action =
      fmap IntMap.fromList . forConcurrently [1 .. 5] $ \phase -> do
        convs <- replicateM n $ action
        pure (phase, convs)

    retry500Once :: App Response -> App Response
    retry500Once action = do
      action `bindResponse` \resp -> do
        if resp.status == 500
          then action
          else pure resp

    runPhaseOperations :: (HasCallStack) => Int -> Value -> String -> TestConvList -> Value -> Value -> App [ConvId]
    runPhaseOperations phase convAdmin tid TestConvList {..} mel mark = do
      withWebSocket mel $ \melWS -> do
        forConcurrently_ (IntMap.findWithDefault [] phase kickMelConvs) $ \convId -> do
          retry500Once (removeMember convAdmin convId mel) >>= assertSuccess

        void $ awaitNMatches n isConvLeaveNotif melWS

        forConcurrently_ (IntMap.findWithDefault [] phase kickMarkConvs) $ \convId -> do
          retry500Once (removeMember convAdmin convId mark) >>= assertSuccess

        void $ awaitNMatches n isConvLeaveNotif melWS

        forConcurrently_ (IntMap.findWithDefault [] phase delConvs) $ \convId -> do
          retry500Once (deleteTeamConversation tid convId convAdmin) >>= assertSuccess

        forConcurrently_ (IntMap.findWithDefault [] phase addMelConvs) $ \convId -> do
          retry500Once (addMembers convAdmin convId (def {users = [mel]})) >>= assertSuccess

        void $ awaitNMatches n isConvDeleteNotif melWS
        replicateConcurrently n
          $ createTestConv convAdmin tid [mel]

    waitForMigration :: (HasCallStack) => String -> App ()
    waitForMigration domainM = do
      metrics <-
        getMetrics domainM BackgroundWorker `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 200
          pure $ Text.decodeUtf8 resp.body
      let (_, _, _, convFinishedMatches) :: (Text, Text, Text, [Text]) = (metrics =~ Text.pack "^wire_local_convs_migration_finished\\ ([0-9]+\\.[0-9]+)$")
      let (_, _, _, userFinishedMatches) :: (Text, Text, Text, [Text]) = (metrics =~ Text.pack "^wire_user_remote_convs_migration_finished\\ ([0-9]+\\.[0-9]+)$")
      when (convFinishedMatches /= [Text.pack "1.0"] || userFinishedMatches /= [Text.pack "1.0"]) $ do
        liftIO $ threadDelay 100_000
        waitForMigration domainM

-- Test Helpers

data TestConvList = TestConvList
  { unmodifiedConvs :: [ConvId],
    kickMelConvs :: IntMap [ConvId],
    kickMarkConvs :: IntMap [ConvId],
    delConvs :: IntMap [ConvId],
    addMelConvs :: IntMap [ConvId]
  }

instance ToJSON TestConvList where
  toJSON convList = do
    object
      [ fromString "unmodifiedConvs" .= (mkId <$> convList.unmodifiedConvs),
        fromString "kickMelConvs" .= (mkId <$$> convList.kickMelConvs),
        fromString "kickMarkConvs" .= (mkId <$$> convList.kickMarkConvs),
        fromString "delConvs" .= (mkId <$$> convList.delConvs),
        fromString "addMelConvs" .= (mkId <$$> convList.addMelConvs)
      ]
    where
      mkId :: ConvId -> String
      mkId cid = cid.id_ <> "@" <> cid.domain

instance Semigroup TestConvList where
  l1 <> l2 =
    TestConvList
      { unmodifiedConvs = l1.unmodifiedConvs <> l2.unmodifiedConvs,
        kickMelConvs = IntMap.unionWith (<>) l1.kickMelConvs l2.kickMelConvs,
        kickMarkConvs = IntMap.unionWith (<>) l1.kickMarkConvs l2.kickMarkConvs,
        delConvs = IntMap.unionWith (<>) l1.delConvs l2.delConvs,
        addMelConvs = IntMap.unionWith (<>) l1.addMelConvs l2.addMelConvs
      }
