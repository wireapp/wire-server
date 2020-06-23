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

module Test.Wire.API.Roundtrip.ByteString (tests) where

import Data.ByteString.Conversion
import Imports
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck (Arbitrary, counterexample, testProperty, (===))
import Type.Reflection (typeRep)
import qualified Wire.API.Arbitrary as Arbitrary ()
import qualified Wire.API.Asset.V3 as Asset.V3
import qualified Wire.API.Asset.V3.Resumable as Asset.V3.Resumable
import qualified Wire.API.Call.TURN as Call.TURN
import qualified Wire.API.Conversation.Code as Conversation.Code
import qualified Wire.API.Conversation.Role as Conversation.Role
import qualified Wire.API.Properties as Properties
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Service as Provider.Service
import qualified Wire.API.Provider.Service.Tag as Provider.Service.Tag
import qualified Wire.API.Push.V2.Token as Push.V2.Token
import qualified Wire.API.Team.Feature as Team.Feature
import qualified Wire.API.User as User
import qualified Wire.API.User.Activation as User.Activation
import qualified Wire.API.User.Auth as User.Auth
import qualified Wire.API.User.Identity as User.Identity
import qualified Wire.API.User.Password as User.Password
import qualified Wire.API.User.Profile as User.Profile

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "JSON roundtrip tests" $
    [ testRoundTrip @Asset.V3.AssetKey,
      testRoundTrip @Asset.V3.AssetRetention,
      testRoundTrip @Asset.V3.AssetToken,
      testRoundTrip @Asset.V3.Resumable.ChunkSize,
      testRoundTrip @Asset.V3.Resumable.Offset,
      testRoundTrip @Asset.V3.Resumable.TotalSize,
      testRoundTrip @Call.TURN.Scheme,
      testRoundTrip @Call.TURN.Transport,
      testRoundTrip @Call.TURN.TurnHost,
      testRoundTrip @Call.TURN.TurnURI,
      testRoundTrip @Conversation.Code.Key,
      testRoundTrip @Conversation.Code.Value,
      testRoundTrip @Conversation.Role.RoleName,
      testRoundTrip @Properties.PropertyKey,
      testRoundTrip @Provider.HttpsUrl,
      testRoundTrip @Provider.Service.ServiceKeyPEM,
      testRoundTrip @Provider.Service.ServiceToken,
      testRoundTrip @Provider.Service.Tag.ServiceTag,
      testRoundTrip @Push.V2.Token.Token,
      testRoundTrip @Team.Feature.TeamFeatureName,
      testRoundTrip @Team.Feature.TeamFeatureStatusValue,
      testRoundTrip @User.Activation.ActivationCode,
      testRoundTrip @User.Activation.ActivationKey,
      testRoundTrip @User.Auth.CookieLabel,
      testRoundTrip @User.Identity.Email,
      testRoundTrip @User.Identity.Phone,
      testRoundTrip @User.InvitationCode,
      testRoundTrip @User.Password.PasswordResetCode,
      testRoundTrip @User.Password.PasswordResetKey,
      testRoundTrip @User.Profile.Name,
      testRoundTrip @(Provider.Service.Tag.QueryAllTags 3 5),
      testRoundTrip @(Provider.Service.Tag.QueryAnyTags 3 5)
      -- FUTUREWORK:
      -- testCase "Call.TURN.TurnUsername (doesn't have FromByteString)" ...
      -- testCase "User.Activation.ActivationTarget (doesn't have FromByteString)" ...
    ]

testRoundTrip ::
  forall a.
  (Arbitrary a, Typeable a, ToByteString a, FromByteString a, Eq a, Show a) =>
  T.TestTree
testRoundTrip = testProperty msg trip
  where
    msg = show (typeRep @a)
    trip (v :: a) =
      counterexample (show $ toByteString' v) $
        Just v === (fromByteString . toByteString') v
