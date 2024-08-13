-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Manual.SendActivationCode_user where

import Data.ISO3166_CountryCodes
import Data.LanguageCodes qualified
import Imports
import Wire.API.User
import Wire.API.User.Activation

testObject_SendActivationCode_1 :: SendActivationCode
testObject_SendActivationCode_1 =
  SendActivationCode
    { emailKey = unsafeEmailAddress "\1021635" "nK",
      locale = Nothing
    }

testObject_SendActivationCode_2 :: SendActivationCode
testObject_SendActivationCode_2 =
  SendActivationCode
    { emailKey = unsafeEmailAddress "b" "4M\1076452P\149723$[\DC2j",
      locale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.CU, lCountry = Just (Country {fromCountry = VI})})
    }
