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
module Test.Wire.API.Golden.Generated.ResumableAsset_user where

import Control.Lens ((.~), (?~))
import Data.Id (Id (Id))
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID (fromString)
import Imports (Functor (fmap), Maybe (Just), fromJust, fromRight, read, undefined, (&))
import Wire.API.Asset
  ( AssetKey (AssetKeyV3),
    AssetRetention
      ( AssetExpiring
      ),
    AssetToken (AssetToken, assetTokenAscii),
    ChunkSize (ChunkSize, chunkSizeBytes),
    ResumableAsset,
    assetExpires,
    assetToken,
    mkAsset,
    mkResumableAsset,
  )

testObject_ResumableAsset_user_1 :: ResumableAsset
testObject_ResumableAsset_user_1 =
  mkResumableAsset
    ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000010-0000-0008-0000-004300000006"))) AssetExpiring)
        & assetExpires .~ fmap read (Just "1864-04-13 11:37:47.393 UTC")
        & assetToken ?~ (AssetToken {assetTokenAscii = fromRight undefined (validate "5A==")})
    )
    (read "1864-04-09 06:01:25.576 UTC")
    (ChunkSize {chunkSizeBytes = 17})
