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

module Test.Wire.API.Golden.Generated.QualifiedUserClientPrekeyMapV4_user where

import Data.Domain (Domain (..))
import Data.Id (ClientId (..), Id (Id), UserId)
import qualified Data.Map as Map
import Data.Qualified (Qualified (..))
import qualified Data.UUID as UUID (fromString)
import Imports
import Wire.API.User.Client (QualifiedUserClientMap (..), QualifiedUserClientPrekeyMapV4 (..))

domain1, domain2 :: Domain
domain1 = Domain "example.com"
domain2 = Domain "test.net"

user1, user2 :: UserId
user1 = Id . fromJust $ UUID.fromString "44f9c51e-0dce-4e7f-85ba-b4e5a545ce68"
user2 = Id . fromJust $ UUID.fromString "284c4e8f-78ef-43f4-a77a-015c22e37960"

clientId :: ClientId
clientId = ClientId "0123456789ABCEF"

testObject_QualifiedUserClientPrekeyMapV4_user_1 :: QualifiedUserClientPrekeyMapV4
testObject_QualifiedUserClientPrekeyMapV4_user_1 =
  QualifiedUserClientPrekeyMapV4
    { qualifiedUserClientPrekeys = QualifiedUserClientMap mempty,
      failedToList = Nothing
    }

testObject_QualifiedUserClientPrekeyMapV4_user_2 :: QualifiedUserClientPrekeyMapV4
testObject_QualifiedUserClientPrekeyMapV4_user_2 =
  QualifiedUserClientPrekeyMapV4
    { qualifiedUserClientPrekeys = QualifiedUserClientMap $ Map.singleton domain1 $ Map.singleton user1 $ Map.singleton clientId Nothing,
      failedToList = Just []
    }

testObject_QualifiedUserClientPrekeyMapV4_user_3 :: QualifiedUserClientPrekeyMapV4
testObject_QualifiedUserClientPrekeyMapV4_user_3 =
  QualifiedUserClientPrekeyMapV4
    { qualifiedUserClientPrekeys = QualifiedUserClientMap mempty,
      failedToList = Just [Qualified user1 domain1, Qualified user2 domain2]
    }
