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

module Test.Connection where

import API.Brig
import API.Galley
import SetupHelpers
import Testlib.Prelude

testConnectWithRemoteUser :: HasCallStack => Domain -> App ()
testConnectWithRemoteUser owningDomain = do
  (alice, bob, one2oneId) <- createOne2OneConversation owningDomain
  aliceId <- alice %. "qualified_id"
  getConversation alice one2oneId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    others <- resp.json %. "members.others" & asList
    qIds <- for others (%. "qualified_id")
    qIds `shouldMatchSet` ([] :: [Value])
  void $ putConnection bob alice "accepted" >>= getBody 200
  getConversation bob one2oneId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    others <- resp.json %. "members.others" & asList
    qIds <- for others (%. "qualified_id")
    qIds `shouldMatchSet` [aliceId]
