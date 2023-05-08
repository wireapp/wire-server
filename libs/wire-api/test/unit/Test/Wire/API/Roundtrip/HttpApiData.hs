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

module Test.Wire.API.Roundtrip.HttpApiData (tests) where

import Imports
import Servant.API
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck (Arbitrary, counterexample, testProperty, (===))
import Type.Reflection (typeRep)
import qualified Wire.API.Routes.Version
import qualified Wire.API.User
import qualified Wire.API.User.Search
import qualified Wire.Arbitrary as Arbitrary ()

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "HttpApiData roundtrip tests" $
    [ testRoundTrip @Wire.API.User.InvitationCode,
      testRoundTrip @Wire.API.User.Search.PagingState,
      testRoundTrip @Wire.API.Routes.Version.Version,
      testRoundTrip @Wire.API.Routes.Version.VersionNumber
    ]

testRoundTrip ::
  forall a.
  (Arbitrary a, Typeable a, ToHttpApiData a, FromHttpApiData a, Eq a, Show a) =>
  T.TestTree
testRoundTrip = testProperty msg trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample (show $ v) $
        Right v === (parseQueryParam . toQueryParam) v
