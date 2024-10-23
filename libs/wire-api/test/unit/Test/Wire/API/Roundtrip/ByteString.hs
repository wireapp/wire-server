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

module Test.Wire.API.Roundtrip.ByteString (tests) where

import Data.ByteString.Conversion
import Imports
import Test.Tasty qualified as T
import Test.Tasty.QuickCheck (Arbitrary, counterexample, testProperty, (===))
import Type.Reflection (typeRep)
import Wire.API.Asset qualified as Asset
import Wire.API.Call.Config qualified as Call.Config
import Wire.API.Conversation.Code qualified as Conversation.Code
import Wire.API.Conversation.Role qualified as Conversation.Role
import Wire.API.OAuth qualified as OAuth
import Wire.API.Properties qualified as Properties
import Wire.API.Provider qualified as Provider
import Wire.API.Provider.Service qualified as Provider.Service
import Wire.API.Provider.Service.Tag qualified as Provider.Service.Tag
import Wire.API.Push.V2.Token qualified as Push.V2.Token
import Wire.API.Team.Feature qualified as Team.Feature
import Wire.API.Team.Role qualified as Team.Role
import Wire.API.User qualified as User
import Wire.API.User.Activation qualified as User.Activation
import Wire.API.User.Auth qualified as User.Auth
import Wire.API.User.Identity qualified as User.Identity
import Wire.API.User.IdentityProvider qualified as User.IdentityProvider
import Wire.API.User.Password qualified as User.Password
import Wire.API.User.Profile qualified as User.Profile
import Wire.API.User.Search qualified as User.Search
import Wire.Arbitrary qualified as Arbitrary ()

tests :: T.TestTree
tests =
  T.localOption (T.Timeout (60 * 1000000) "60s") . T.testGroup "ByteString roundtrip tests" $
    [ testRoundTrip @Asset.AssetKey,
      testRoundTrip @Asset.AssetRetention,
      testRoundTrip @Asset.AssetToken,
      testRoundTrip @Call.Config.Scheme,
      testRoundTrip @Call.Config.Transport,
      testRoundTrip @Call.Config.TurnHost,
      testRoundTrip @Call.Config.TurnURI,
      testRoundTrip @Conversation.Code.Key,
      testRoundTrip @Conversation.Code.Value,
      testRoundTrip @Conversation.Role.RoleName,
      testRoundTrip @Properties.PropertyKey,
      testRoundTrip @Provider.HttpsUrl,
      testRoundTrip @Provider.Service.ServiceKeyPEM,
      testRoundTrip @Provider.Service.ServiceToken,
      testRoundTrip @(Provider.Service.Tag.QueryAllTags 3 5),
      testRoundTrip @(Provider.Service.Tag.QueryAnyTags 3 5),
      testRoundTrip @Provider.Service.Tag.ServiceTag,
      testRoundTrip @Push.V2.Token.Token,
      testRoundTrip @Team.Feature.FeatureTTL,
      testRoundTrip @Team.Feature.FeatureStatus,
      testRoundTrip @User.Activation.ActivationCode,
      testRoundTrip @User.Activation.ActivationKey,
      testRoundTrip @User.Auth.CookieLabel,
      testRoundTrip @User.Identity.EmailAddress,
      testRoundTrip @User.Identity.Phone,
      testRoundTrip @User.InvitationCode,
      testRoundTrip @User.Password.PasswordResetCode,
      testRoundTrip @User.Password.PasswordResetKey,
      testRoundTrip @User.Profile.ManagedBy,
      testRoundTrip @User.Profile.Name,
      testRoundTrip @Team.Role.Role,
      testRoundTrip @User.Search.PagingState,
      testRoundTrip @User.Search.TeamUserSearchSortBy,
      testRoundTrip @User.Search.TeamUserSearchSortOrder,
      testRoundTrip @User.Search.RoleFilter,
      testRoundTrip @User.IdentityProvider.WireIdPAPIVersion,
      testRoundTrip @OAuth.OAuthScope
      -- FUTUREWORK:
      -- testCase "Call.Config.TurnUsername (doesn't have FromByteString)" ...
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
