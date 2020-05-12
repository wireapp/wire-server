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

module Test.Brig.Types.User where

import Brig.Types.Activation
import Brig.Types.Intra
import Brig.Types.Provider (UpdateServiceWhitelist)
import Brig.Types.Team.Invitation
import Brig.Types.Test.Arbitrary ()
import Brig.Types.User
import Imports
import Test.Brig.Roundtrip
import Test.QuickCheck
import Test.Tasty
import Wire.API.Arbitrary ()
import Wire.API.Team.Member

tests :: TestTree
tests = testGroup "User (types vs. aeson)" $ roundtripTests

roundtripTests :: [TestTree]
roundtripTests =
  [ testRoundTrip @BindingNewTeamUser,
    testRoundTrip @CheckHandles,
    testRoundTrip @CompletePasswordReset,
    testRoundTrip @DeleteUser,
    testRoundTrip @DeletionCodeTimeout,
    testRoundTrip @EmailUpdate,
    testRoundTrip @HandleUpdate,
    testRoundTrip @InvitationList,
    testRoundTrip @Invitation,
    testRoundTrip @InvitationRequest,
    testRoundTrip @LocaleUpdate,
    testRoundTrip @NewPasswordReset,
    testRoundTrip @NewUser,
    testRoundTrip @PasswordChange,
    testRoundTrip @PhoneUpdate,
    testRoundTrip @ManagedByUpdate,
    testRoundTrip @ReAuthUser,
    testRoundTrip @SelfProfile,
    testRoundTrip @TeamMember,
    testRoundTrip @UpdateServiceWhitelist,
    testRoundTrip @UserHandleInfo,
    testRoundTrip @UserIdentity,
    testRoundTrip @UserProfile,
    testRoundTrip @User,
    testRoundTrip @RichInfo,
    testRoundTrip @UserUpdate,
    testRoundTrip @RichInfoUpdate,
    testRoundTrip @VerifyDeleteUser
  ]

instance Arbitrary ManagedByUpdate where
  arbitrary = ManagedByUpdate <$> arbitrary

instance Arbitrary RichInfoUpdate where
  arbitrary = RichInfoUpdate <$> arbitrary

instance Arbitrary ReAuthUser where
  arbitrary = ReAuthUser <$> arbitrary
