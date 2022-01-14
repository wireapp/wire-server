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

-- Test objects for types in Wire.API.Provider

module Test.Wire.API.Golden.Manual.Provider where

import Data.ByteString.Conversion
import Data.Id (Id (..))
import Data.Misc (PlainTextPassword (..))
import Data.Range (unsafeRange)
import qualified Data.UUID as UUID
import Imports
import Wire.API.Provider
import Wire.API.User.Identity (Email (..))
import Wire.API.User.Profile (Name (..))

someId :: forall a. Id a
someId = Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))

testObject_Provider1 :: Provider
testObject_Provider1 =
  Provider
    { providerId = someId,
      providerName = Name "provider-name",
      providerEmail = Email "test" "example.com",
      providerUrl = fromJust $ fromByteString' "https://www.example.com/meh",
      providerDescr = "description of provider"
    }

testObject_NewProvider1 :: NewProvider
testObject_NewProvider1 =
  NewProvider
    { newProviderName = Name "new provider name",
      newProviderEmail = Email "test" "example.com",
      newProviderUrl = fromJust $ fromByteString' "https://www.example.com/meh",
      newProviderDescr = unsafeRange "description of provider",
      newProviderPassword = Nothing
    }

testObject_NewProvider2 :: NewProvider
testObject_NewProvider2 =
  NewProvider
    { newProviderName = Name "new provider name",
      newProviderEmail = Email "test" "example.com",
      newProviderUrl = fromJust $ fromByteString' "https://www.example.com/meh",
      newProviderDescr = unsafeRange "description of provider",
      newProviderPassword = Just (PlainTextPassword "hunter5")
    }
