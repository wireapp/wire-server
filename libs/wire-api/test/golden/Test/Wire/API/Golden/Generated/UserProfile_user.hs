{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.UserProfile_user where

import Data.Domain
import Data.Either.Combinators
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.LegalHold
import Data.Qualified
import Data.UUID qualified as UUID
import Imports
import Wire.API.Provider.Service
import Wire.API.User

testObject_UserProfile_user_1 :: UserProfile
testObject_UserProfile_user_1 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000")),
            qDomain = Domain {_domainText = "v.ay64d"}
          },
      profileName = Name {fromName = "\50534\3354]$\169938\183604UV`\nF\f\23427ys'd\bXy\ENQ:\ESC\139288\RSD[<\132982E"},
      profileTextStatus = rightToMaybe $ mkTextStatus "text status",
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = 2},
      profileDeleted = False,
      profileService = Nothing,
      profileHandle = Nothing,
      profileExpire = Nothing,
      profileTeam = Nothing,
      profileEmail = Nothing,
      profileLegalholdStatus = UserLegalHoldDisabled,
      profileSupportedProtocols = defSupportedProtocols
    }

testObject_UserProfile_user_2 :: UserProfile
testObject_UserProfile_user_2 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001")),
            qDomain = Domain {_domainText = "go.7.w-3r8iy2.a"}
          },
      profileName = Name {fromName = "si4v\999679\ESC^'\12447k\21889\NAK?\1082547\NULBw;\b3*R/\164149lrI"},
      profileTextStatus = Nothing,
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = -1},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
                _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))
              }
          ),
      profileHandle =
        Just (fromJust (parseHandle "emsonpvo3-x_4ys4qjtjtkfgx.mag6pi2ldq.77m5vnsn_tte41r-0vwgklpeejr1t4se0bknu4tsuqs-njzh34-ba_mj8lm5x6aro4o.2wsqe0ldx")),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T01:42:22.437Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),
      profileEmail = Just (unsafeEmailAddress "\172353 " ""),
      profileLegalholdStatus = UserLegalHoldNoConsent,
      profileSupportedProtocols = defSupportedProtocols
    }
