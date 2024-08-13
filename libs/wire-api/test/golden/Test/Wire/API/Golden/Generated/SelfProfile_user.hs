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

module Test.Wire.API.Golden.Generated.SelfProfile_user where

import Data.Domain
import Data.Either.Combinators (rightToMaybe)
import Data.Handle
import Data.ISO3166_CountryCodes
import Data.Id
import Data.Json.Util
import Data.LanguageCodes qualified
import Data.Qualified
import Data.UUID qualified as UUID
import Imports
import Wire.API.Provider.Service
import Wire.API.User

testObject_SelfProfile_user_1 :: SelfProfile
testObject_SelfProfile_user_1 =
  SelfProfile
    { selfUser =
        User
          { userQualifiedId =
              Qualified
                { qUnqualified = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002")),
                  qDomain = Domain {_domainText = "n0-994.m-226.f91.vg9p-mj-j2"}
                },
            userIdentity =
              Just (EmailIdentity (unsafeEmailAddress "\a" "")),
            userDisplayName = Name {fromName = "@\1457\2598\66242\US\1104967l+\137302\&6\996495^\162211Mu\t"},
            userTextStatus = rightToMaybe $ mkTextStatus "text status",
            userPict = Pict {fromPict = []},
            userAssets = [],
            userAccentId = ColourId {fromColourId = 1},
            userDeleted = False,
            userLocale =
              Locale {lLanguage = Language Data.LanguageCodes.GL, lCountry = Just (Country {fromCountry = PA})},
            userService =
              Just
                ( ServiceRef
                    { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
                      _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))
                    }
                ),
            userHandle = Just (fromJust (parseHandle "do9-5")),
            userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T21:09:29.342Z")),
            userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))),
            userManagedBy = ManagedByScim,
            userSupportedProtocols = defSupportedProtocols
          }
    }
