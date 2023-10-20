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

testAddUsersSomeReachable :: HasCallStack => App ()
testAddUsersSomeReachable = do
  (addCommit, d) <- startDynamicBackends [mempty] $ \[thirdDomain] -> do
    ownDomain <- make OwnDomain & asString
    otherDomain <- make OtherDomain & asString
    [alice, bob, charlie] <- createAndConnectUsers [ownDomain, otherDomain, thirdDomain]

    [alice1, bob1, charlie1] <- traverse (createMLSClient def) [alice, bob, charlie]
    traverse_ uploadNewKeyPackage [bob1, charlie1]
    void $ createNewGroup alice1
    void $ withWebSocket bob $ \ws -> do
      void $ createAddCommit alice1 [bob] >>= sendAndConsumeCommitBundle
      awaitMatch 10 isMemberJoinNotif ws
    mp <- createAddCommit alice1 [charlie]
    pure (mp, thirdDomain)

  -- try adding Charlie now that his backend is unreachable
  bindResponse (postMLSCommitBundle addCommit.sender (mkBundle addCommit)) $ \resp -> do
    resp.status `shouldMatchInt` 533
    (resp.json %. "unreachable_backends" & asList) `shouldMatch` [d]

-- There is analogous counterpart for Proteus in the 'Test.Conversation' module.
testAddReachableWithUnreachableRemoteUsers :: HasCallStack => App ()
testAddReachableWithUnreachableRemoteUsers = do
  resourcePool <- asks resourcePool
  runCodensity (acquireResources 1 resourcePool) $ \[cDom] -> do
    (alice1, bob) <- runCodensity (startDynamicBackend cDom mempty) $ \_ -> do
      ownDomain <- make OwnDomain & asString
      [alice, charlie] <- createAndConnectUsers [ownDomain, cDom.berDomain]

      [alice1, charlie1] <- traverse (createMLSClient def) [alice, charlie]
      void $ uploadNewKeyPackage charlie1
      void $ createNewGroup alice1
      void $ withWebSocket charlie $ \ws -> do
        void $ createAddCommit alice1 [charlie] >>= sendAndConsumeCommitBundle
        awaitMatch 10 isMemberJoinNotif ws
      otherDomain <- make OtherDomain & asString
      bob <- randomUser otherDomain def
      forM_ [alice, charlie] $ connectTwoUsers bob
      pure (alice1, bob)

    bob1 <- createMLSClient def bob
    void $ uploadNewKeyPackage bob1
    mp <- createAddCommit alice1 [bob]
    bindResponse (postMLSCommitBundle mp.sender (mkBundle mp)) $ \resp -> do
      resp.status `shouldMatchInt` 533
      resp.jsonBody %. "unreachable_backends" `shouldMatchSet` [cDom.berDomain]
