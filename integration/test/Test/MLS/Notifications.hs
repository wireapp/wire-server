-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.MLS.Notifications where

import API.Gundeck
import MLS.Util
import Notifications
import SetupHelpers
import Testlib.Prelude

testWelcomeNotification :: (HasCallStack) => App ()
testWelcomeNotification = do
  [alice, bob] <- createAndConnectUsers [OwnDomain, OtherDomain]
  [alice1, alice2, bob1, bob2] <- traverse (createMLSClient def) [alice, alice, bob, bob]
  traverse_ (uploadNewKeyPackage def) [alice2, bob1, bob2]

  convId <- createNewGroup def alice1
  notif <- withWebSocket bob $ \ws -> do
    void $ createAddCommit alice1 convId [alice, bob] >>= sendAndConsumeCommitBundle
    awaitMatch isWelcomeNotif ws

  notifId <- notif %. "id" & asString

  for_ [bob1, bob2] $ \cid ->
    getNotifications
      bob
      def
        { since = Just notifId,
          client = Just cid.client,
          size = Just 10000
        }
      >>= getJSON 200
