{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Brig.Types.User where

import Brig.Types.Connection (UpdateConnectionsInternal (..))
import Brig.Types.Intra (NewUserScimInvitation (..), UserAccount (..))
import Brig.Types.Search (SearchVisibilityInbound (..))
import Brig.Types.User (ManagedByUpdate (..), RichInfoUpdate (..))
import Data.Aeson
import Imports
import Test.Brig.Roundtrip (testRoundTrip, testRoundTripWithSwagger)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Routes.Internal.Brig.EJPD (EJPDRequestBody (..), EJPDResponseBody (..))
import Wire.API.User.Auth.ReAuth

tests :: TestTree
tests = testGroup "User (types vs. aeson)" $ roundtripTests

roundtripTests :: [TestTree]
roundtripTests =
  [ testRoundTrip @ManagedByUpdate,
    testRoundTrip @ReAuthUser,
    testRoundTrip @RichInfoUpdate,
    testRoundTrip @NewUserScimInvitation,
    testRoundTripWithSwagger @EJPDRequestBody,
    testRoundTripWithSwagger @EJPDResponseBody,
    testRoundTrip @UpdateConnectionsInternal,
    testRoundTrip @SearchVisibilityInbound,
    testRoundTripWithSwagger @UserAccount,
    testGroup "golden tests" $
      [testCaseUserAccount]
  ]

instance Arbitrary ReAuthUser where
  arbitrary = ReAuthUser <$> arbitrary <*> arbitrary <*> arbitrary

testCaseUserAccount :: TestTree
testCaseUserAccount = testCase "UserAcccount" $ do
  assertEqual "1" (Just json1) (encode <$> decode @UserAccount json1)
  assertEqual "2" (Just json2) (encode <$> decode @UserAccount json2)
  where
    json1 :: LByteString
    json1 = "{\"accent_id\":1,\"assets\":[],\"deleted\":true,\"email\":\"foo@example.com\",\"expires_at\":\"1864-05-09T17:20:22.192Z\",\"handle\":\"-ve\",\"id\":\"00000000-0000-0001-0000-000100000000\",\"locale\":\"lu\",\"managed_by\":\"wire\",\"name\":\"bla\",\"picture\":[],\"qualified_id\":{\"domain\":\"4-o60.j7-i\",\"id\":\"00000000-0000-0001-0000-000100000000\"},\"service\":{\"id\":\"00000000-0000-0001-0000-000000000001\",\"provider\":\"00000001-0000-0001-0000-000000000001\"},\"status\":\"suspended\",\"supported_protocols\":[\"proteus\"],\"team\":\"00000000-0000-0001-0000-000100000001\"}"

    json2 :: LByteString
    json2 = "{\"accent_id\":0,\"assets\":[{\"key\":\"3-4-00000000-0000-0001-0000-000000000000\",\"size\":\"preview\",\"type\":\"image\"}],\"email\":\"a@b\",\"expires_at\":\"1864-05-10T22:45:44.823Z\",\"handle\":\"b8m\",\"id\":\"00000000-0000-0000-0000-000000000001\",\"locale\":\"tk-KZ\",\"managed_by\":\"wire\",\"name\":\"name2\",\"picture\":[],\"qualified_id\":{\"domain\":\"1-8wq0.b22k1.w5\",\"id\":\"00000000-0000-0000-0000-000000000001\"},\"service\":{\"id\":\"00000000-0000-0001-0000-000000000001\",\"provider\":\"00000001-0000-0001-0000-000100000000\"},\"status\":\"pending-invitation\",\"supported_protocols\":[\"proteus\"],\"team\":\"00000000-0000-0001-0000-000000000001\"}"
