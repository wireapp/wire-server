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

import MLS.Util
import SetupHelpers
import Testlib.Prelude

-- | Every MLS commit acquires and releases the commit lock, so two successive
-- commits prove acquire -> release -> re-acquire through the pg advisory-lock
-- interpreter. A leaked lock would fail the second commit.
testMLSCommitLock :: (HasCallStack) => App ()
testMLSCommitLock = do
  alice <- randomUser OwnDomain def
  alice1 <- createMLSClient def alice
  bob <- randomUser OwnDomain def
  bob1 <- createMLSClient def bob
  void $ uploadNewKeyPackage def bob1
  convId <- createNewGroup def alice1
  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle

  charlie <- randomUser OwnDomain def
  charlie1 <- createMLSClient def charlie
  void $ uploadNewKeyPackage def charlie1
  void $ createAddCommit alice1 convId [charlie] >>= sendAndConsumeCommitBundle
