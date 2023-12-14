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

module Test.Wire.API.Swagger (tests) where

import Data.Aeson (ToJSON)
import Data.OpenApi (ToSchema, validatePrettyToJSON)
import Imports
import Test.Tasty qualified as T
import Test.Tasty.QuickCheck (Arbitrary, counterexample, testProperty)
import Type.Reflection (typeRep)
import Wire.API.User qualified as User
import Wire.API.User.Client qualified as Client
import Wire.API.User.Client.Prekey qualified as Prekey
import Wire.API.User.Handle qualified as Handle
import Wire.API.User.Search qualified as Search
import Wire.API.UserMap qualified as UserMap
import Wire.API.Wrapped qualified as Wrapped

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "JSON roundtrip tests" $
    [ testToJSON @User.UserProfile,
      testToJSON @User.User,
      testToJSON @User.SelfProfile,
      testToJSON @(User.LimitedQualifiedUserIdList 20),
      testToJSON @Handle.UserHandleInfo,
      testToJSON @Client.Client,
      testToJSON @Client.PubClient,
      testToJSON @(UserMap.UserMap (Set Client.Client)),
      testToJSON @(UserMap.UserMap (Set Client.PubClient)),
      testToJSON @(UserMap.QualifiedUserMap (Set Client.Client)),
      testToJSON @Client.UserClientPrekeyMap,
      testToJSON @Client.UserClients,
      testToJSON @Prekey.Prekey,
      testToJSON @Prekey.PrekeyBundle,
      testToJSON @Prekey.ClientPrekey,
      testToJSON @Client.QualifiedUserClientPrekeyMap,
      testToJSON @Client.QualifiedUserClients,
      testToJSON @Search.Contact,
      testToJSON @(Search.SearchResult Search.Contact),
      testToJSON @(Wrapped.Wrapped "some_user" User.User)
    ]

testToJSON :: forall a. (Arbitrary a, ToJSON a, ToSchema a, Show a) => T.TestTree
testToJSON = testProperty msg trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample
        ( fromMaybe "Schema validation failed, but there were no errors. This looks like a bug in swagger2!" $
            validatePrettyToJSON v
        )
        $ isNothing (validatePrettyToJSON v)
