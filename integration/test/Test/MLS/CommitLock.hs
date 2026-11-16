-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.MLS.CommitLock where

import API.Galley
import MLS.Util
import SetupHelpers
import Testlib.Prelude

-- | A commit that is rejected after the commit lock for its (group, epoch)
-- has been taken must release that lock: a valid commit for the very same
-- (group, epoch) has to go through afterwards. If the lock leaked, the second
-- commit would be rejected with @mls-stale-message@.
testMLSCommitLockReleasedOnFailure :: (HasCallStack) => App ()
testMLSCommitLockReleasedOnFailure = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OwnDomain]
  [alice1, bob1, bob2] <- traverse (createMLSClient def) [alice, bob, bob]
  traverse_ (uploadNewKeyPackage def) [bob1, bob2]
  convId <- createNewGroup def alice1
  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle

  gs <- getClientGroupState alice1

  -- Removing only some of bob's clients is rejected with the commit lock held.
  mp <- createRemoveCommit alice1 convId [bob1]
  err <- postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 409
  err %. "label" `shouldMatch` "mls-client-mismatch"

  -- Same group, same epoch: this needs the same lock again.
  setClientGroupState alice1 gs
  void $ createRemoveCommit alice1 convId [bob1, bob2] >>= sendAndConsumeCommitBundle
