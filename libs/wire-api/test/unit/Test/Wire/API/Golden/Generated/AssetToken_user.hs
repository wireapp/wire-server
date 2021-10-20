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
module Test.Wire.API.Golden.Generated.AssetToken_user where

import Data.Text.Ascii (AsciiChars (validate))
import Imports (fromRight, undefined)
import Wire.API.Asset (AssetToken (..))

testObject_AssetToken_user_1 :: AssetToken
testObject_AssetToken_user_1 = AssetToken {assetTokenAscii = fromRight undefined (validate "19gg")}

testObject_AssetToken_user_4 :: AssetToken
testObject_AssetToken_user_4 =
  AssetToken {assetTokenAscii = fromRight undefined (validate "NMWuAr4DWSWYN1yQACS39YW-")}

testObject_AssetToken_user_5 :: AssetToken
testObject_AssetToken_user_5 = AssetToken {assetTokenAscii = fromRight undefined (validate "1M4G-RUbZgqvkfQ=")}

testObject_AssetToken_user_7 :: AssetToken
testObject_AssetToken_user_7 =
  AssetToken {assetTokenAscii = fromRight undefined (validate "3Z8JnP8YkWspJHvxbfa3fd2VyR9S_PMcVtN-HA==")}

testObject_AssetToken_user_12 :: AssetToken
testObject_AssetToken_user_12 =
  AssetToken {assetTokenAscii = fromRight undefined (validate "CD3Zk7PTkfn816hCz42NE41KpPd4")}

testObject_AssetToken_user_15 :: AssetToken
testObject_AssetToken_user_15 =
  AssetToken {assetTokenAscii = fromRight undefined (validate "G7HpRJ4qnEkkbbswnSW9SAEZtXI9vI9MgQ==")}

testObject_AssetToken_user_20 :: AssetToken
testObject_AssetToken_user_20 =
  AssetToken {assetTokenAscii = fromRight undefined (validate "W2jwr0ImzE7IaMvETUb5Bt1rn3E=")}
