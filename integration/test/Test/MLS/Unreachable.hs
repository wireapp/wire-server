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

module Test.MLS.Unreachable where

import API.Galley
import Control.Monad.Codensity
import Control.Monad.Reader
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool

testAddUsersSomeReachable :: (HasCallStack) => App ()
testAddUsersSomeReachable = do
  (addCommit, d) <- startDynamicBackends [mempty] $ \[thirdDomain] -> do
    ownDomain <- make OwnDomain & asString
    otherDomain <- make OtherDomain & asString
    [alice, bob, charlie] <- createAndConnectUsers [ownDomain, otherDomain, thirdDomain]

    [alice1, bob1, charlie1] <- traverse (createMLSClient def def) [alice, bob, charlie]
    traverse_ (uploadNewKeyPackage def) [bob1, charlie1]
    convId <- createNewGroup def alice1
    void $ withWebSocket bob $ \ws -> do
      void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle
      awaitMatch isMemberJoinNotif ws
    mp <- createAddCommit alice1 convId [charlie]
    pure (mp, thirdDomain)

  -- try adding Charlie now that his backend is unreachable
  bindResponse (postMLSCommitBundle addCommit.sender (mkBundle addCommit)) $ \resp -> do
    resp.status `shouldMatchInt` 533
    (resp.json %. "unreachable_backends" & asList) `shouldMatch` [d]

-- | There is analogous counterpart for Proteus in the 'Test.Conversation' module.
testAddUserWithUnreachableRemoteUsers :: (HasCallStack) => App ()
testAddUserWithUnreachableRemoteUsers = do
  resourcePool <- asks resourcePool
  runCodensity (acquireResources 1 resourcePool) $ \[cDom] -> do
    (alice1, bob, brad, chris, convId) <- runCodensity (startDynamicBackend cDom mempty) $ \_ -> do
      [own, other] <- forM [OwnDomain, OtherDomain] $ asString . make
      [alice, bob, brad, charlie, chris] <-
        createAndConnectUsers [own, other, other, cDom.berDomain, cDom.berDomain]
      [alice1, charlie1, chris1] <-
        traverse (createMLSClient def def) [alice, charlie, chris]
      traverse_ (uploadNewKeyPackage def) [charlie1, chris1]
      convId <- createNewGroup def alice1
      void $ withWebSocket charlie $ \ws -> do
        void $ createAddCommit alice1 convId [charlie] >>= sendAndConsumeCommitBundle
        awaitMatch isMemberJoinNotif ws
      pure (alice1, bob, brad, chris, convId)

    [bob1, brad1] <- traverse (createMLSClient def def) [bob, brad]
    traverse_ (uploadNewKeyPackage def) [bob1, brad1]

    do
      mp <- createAddCommit alice1 convId [bob]
      bindResponse (postMLSCommitBundle mp.sender (mkBundle mp)) $ \resp -> do
        resp.status `shouldMatchInt` 533
        resp.jsonBody %. "unreachable_backends" `shouldMatchSet` [cDom.berDomain]

      runCodensity (startDynamicBackend cDom mempty) $ \_ ->
        void $ postMLSCommitBundle mp.sender (mkBundle mp) >>= getBody 201

    do
      mp <- createAddCommit alice1 convId [brad]
      void $ postMLSCommitBundle mp.sender (mkBundle mp) >>= getBody 201

    do
      mp <- runCodensity (startDynamicBackend cDom mempty) $ \_ ->
        createAddCommit alice1 convId [chris]
      bindResponse (postMLSCommitBundle mp.sender (mkBundle mp)) $ \resp -> do
        resp.status `shouldMatchInt` 533
        resp.jsonBody %. "unreachable_backends" `shouldMatchSet` [cDom.berDomain]

testAddUnreachableUserFromFederatingBackend :: (HasCallStack) => App ()
testAddUnreachableUserFromFederatingBackend = do
  resourcePool <- asks resourcePool
  runCodensity (acquireResources 1 resourcePool) $ \[cDom] -> do
    mp <- runCodensity (startDynamicBackend cDom mempty) $ \_ -> do
      ownDomain <- make OwnDomain & asString
      otherDomain <- make OtherDomain & asString
      [alice, bob, charlie, chad] <-
        createAndConnectUsers [ownDomain, otherDomain, cDom.berDomain, cDom.berDomain]

      [alice1, bob1, charlie1, chad1] <- traverse (createMLSClient def def) [alice, bob, charlie, chad]
      traverse_ (uploadNewKeyPackage def) [bob1, charlie1, chad1]
      convId <- createNewGroup def alice1
      withWebSockets [bob, charlie] $ \wss -> do
        void $ createAddCommit alice1 convId [bob, charlie] >>= sendAndConsumeCommitBundle
        forM_ wss $ awaitMatch isMemberJoinNotif
      createAddCommit alice1 convId [chad]

    bindResponse (postMLSCommitBundle mp.sender (mkBundle mp)) $ \resp -> do
      resp.status `shouldMatchInt` 533
      resp.jsonBody %. "unreachable_backends" `shouldMatchSet` [cDom.berDomain]
