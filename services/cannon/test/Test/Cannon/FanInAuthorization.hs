-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Cannon.FanInAuthorization (tests) where

import Cannon.NotificationFanIn (mutuallyAcceptedConnections)
import Data.Domain (Domain (..))
import Data.Id
import Data.Qualified (Qualified (..))
import Data.UUID qualified as UUID
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Connection (Relation (..))
import Wire.API.Routes.Internal.Brig.Connection (ConnectionStatusV2 (..))

tests :: TestTree
tests =
  testGroup
    "Fan-in connection authorization"
    [ testCase "mutually accepted local peers are readable" $
        checkPeers [accepted] [ConnectionStatusV2 other self Accepted] @?= [peer],
      testCase "a peer's block revokes reads even while the reader still sees Accepted" $
        checkPeers [accepted] [ConnectionStatusV2 other self Blocked] @?= [],
      testCase "a reader's block also revokes reads" $
        checkPeers [ConnectionStatusV2 uid peer Blocked] [ConnectionStatusV2 other self Accepted] @?= [],
      testCase "reverse acceptance must point to the authenticated reader" $
        checkPeers [accepted] [ConnectionStatusV2 other peer Accepted] @?= [],
      testCase "remote acceptance alone does not authorize profile-stream reads" $
        checkPeers [ConnectionStatusV2 uid (Qualified other (Domain "remote.example.com")) Accepted] [] @?= []
    ]
  where
    domain = Domain "local.example.com"
    uid = Id UUID.nil
    other = Id (UUID.fromWords 0 0 0 1)
    self = Qualified uid domain
    peer = Qualified other domain
    accepted = ConnectionStatusV2 uid peer Accepted
    checkPeers = mutuallyAcceptedConnections domain uid
