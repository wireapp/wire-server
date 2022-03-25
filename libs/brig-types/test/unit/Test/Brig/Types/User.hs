{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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
import Brig.Types.Intra (NewUserScimInvitation (..), ReAuthUser (..))
import Brig.Types.Search (SearchVisibilityInbound (..))
import Brig.Types.User (ManagedByUpdate (..), RichInfoUpdate (..))
import Imports
import Test.Brig.Roundtrip (testRoundTrip, testRoundTripWithSwagger)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.Tasty
import Wire.API.Routes.Internal.Brig.EJPD (EJPDRequestBody (..), EJPDResponseBody (..))

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
    testRoundTrip @SearchVisibilityInbound
  ]

instance Arbitrary ManagedByUpdate where
  arbitrary = ManagedByUpdate <$> arbitrary

instance Arbitrary RichInfoUpdate where
  arbitrary = RichInfoUpdate <$> arbitrary

instance Arbitrary ReAuthUser where
  arbitrary = ReAuthUser <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary NewUserScimInvitation where
  arbitrary = NewUserScimInvitation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
