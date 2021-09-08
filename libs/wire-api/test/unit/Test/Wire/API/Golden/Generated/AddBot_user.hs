-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.AddBot_user where

import Data.ISO3166_CountryCodes (CountryCode (GR))
import Data.Id (Id (Id))
import qualified Data.LanguageCodes (ISO639_1 (UZ))
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation.Bot (AddBot (..))
import Wire.API.User (Country (Country, fromCountry), Language (Language), Locale (Locale, lCountry, lLanguage))

testObject_AddBot_user_1 :: AddBot
testObject_AddBot_user_1 =
  AddBot
    { addBotProvider = Id (fromJust (UUID.fromString "00000015-0000-0010-0000-00110000000d")),
      addBotService = Id (fromJust (UUID.fromString "00000010-0000-000c-0000-001800000010")),
      addBotLocale = Nothing
    }

testObject_AddBot_user_2 :: AddBot
testObject_AddBot_user_2 =
  AddBot
    { addBotProvider = Id (fromJust (UUID.fromString "00000000-0000-0014-0000-00060000001e")),
      addBotService = Id (fromJust (UUID.fromString "00000005-0000-0018-0000-000600000005")),
      addBotLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.UZ, lCountry = Just (Country {fromCountry = GR})})
    }
