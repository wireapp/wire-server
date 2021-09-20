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
module Test.Wire.API.Golden.Generated.NewAssetToken_user where

import Data.Text.Ascii (AsciiChars (validate))
import Imports (fromRight, undefined)
import Wire.API.Asset (AssetToken (AssetToken, assetTokenAscii), NewAssetToken (..))

testObject_NewAssetToken_user_3 :: NewAssetToken
testObject_NewAssetToken_user_3 =
  NewAssetToken
    { newAssetToken =
        AssetToken {assetTokenAscii = fromRight undefined (validate "v6nnEvDMoXZfLJMZyS_Bg9daSGCLnG9Tgw==")}
    }

testObject_NewAssetToken_user_6 :: NewAssetToken
testObject_NewAssetToken_user_6 =
  NewAssetToken
    { newAssetToken =
        AssetToken {assetTokenAscii = fromRight undefined (validate "jtgylSRb-AxTo1u8M5kEKPJ7RbzGf3c=")}
    }

testObject_NewAssetToken_user_9 :: NewAssetToken
testObject_NewAssetToken_user_9 =
  NewAssetToken
    { newAssetToken = AssetToken {assetTokenAscii = fromRight undefined (validate "i4dAbdQ3A9zQE5txWTo3q-nYIhZ1")}
    }
