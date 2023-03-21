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
import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import Data.Qualified (Qualified (..))
import qualified Data.UUID as UUID (fromString)
import Imports
import Wire.API.Connection
  ( Relation (..),
    UserConnection (..),
  )

testObject_QualifiedUserClientPrekeyMapV4_user_1 :: QualifiedUserClientPrekeyMapV4
testObject_QualifiedUserClientPrekeyMapV4_user_1 =
  QualifiedUserClientPrekeyMapV4
    { qualifiedUserClientPrekeys = QualifiedUserClientMap mempty
    , failedToList = Nothing
    }
testObject_QualifiedUserClientPrekeyMapV4_user_2 :: QualifiedUserClientPrekeyMapV4
testObject_QualifiedUserClientPrekeyMapV4_user_2 =
  QualifiedUserClientPrekeyMapV4
    { qualifiedUserClientPrekeys = QualifiedUserClientMap $ Map.singleton (Domain _) $ Map.singleton (UserId _) $ Map.singleton (ClientId _) _
    , failedToList = Just []
    }
testObject_QualifiedUserClientPrekeyMapV4_user_3 :: QualifiedUserClientPrekeyMapV4
testObject_QualifiedUserClientPrekeyMapV4_user_3 =
  QualifiedUserClientPrekeyMapV4
    { qualifiedUserClientPrekeys = QualifiedUserClientMap mempty
    , failedToList = Just [Qualified (UserId _) (Domain _), Qualified (UserId _) (Domain _)]
    }