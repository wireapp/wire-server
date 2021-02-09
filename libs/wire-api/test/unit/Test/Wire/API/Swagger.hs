-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Swagger (ToSchema, validatePrettyToJSON)
import Imports
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck (Arbitrary, counterexample, testProperty)
import Type.Reflection (typeRep)
import qualified Wire.API.User as User
import qualified Wire.API.User.Client as Client
import qualified Wire.API.User.Client.Prekey as Prekey
import qualified Wire.API.User.Handle as Handle
import qualified Wire.API.UserMap as UserMap

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "JSON roundtrip tests" $
    [ testToJSON @User.UserProfile,
      testToJSON @User.User,
      testToJSON @User.SelfProfile,
      testToJSON @Handle.UserHandleInfo,
      testToJSON @Client.Client,
      testToJSON @(Client.UserClientMap (Maybe Prekey.Prekey)),
      testToJSON @Client.UserClients,
      testToJSON @(UserMap.UserMap (Set Client.Client)),
      testToJSON @(UserMap.QualifiedUserMap (Set Client.Client)),
      testToJSON @Prekey.Prekey,
      testToJSON @Prekey.PrekeyBundle,
      testToJSON @Prekey.ClientPrekey,
      testToJSON @(Client.QualifiedUserClientMap (Maybe Prekey.Prekey)),
      testToJSON @Client.QualifiedUserClients
    ]

testToJSON :: forall a. (Arbitrary a, Typeable a, ToJSON a, ToSchema a, Show a) => T.TestTree
testToJSON = testProperty msg trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample
        ( fromMaybe "Schema validation failed, but there were no errors. This looks like a bug in swagger2!" $
            validatePrettyToJSON v
        )
        $ isNothing (validatePrettyToJSON v)
