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

module CargoHold.Types.V3
  ( -- * Principal
    Principal (..),

    -- * Body Construction (re-export)
    buildMultipartBody,
    beginMultipartBody,
    endMultipartBody,

    -- * AssetHeaders (re-export)
    AssetHeaders (..),
    mkHeaders,

    -- * AssetSettings (re-export)
    AssetSettings,
    defAssetSettings,
    setAssetPublic,
    setAssetRetention,
    AssetRetention (..),
    assetRetentionSeconds,
    assetExpiringSeconds,
    assetVolatileSeconds,
    retentionToTextRep,

    -- * AssetToken (re-export)
    AssetToken (..),
    NewAssetToken (..),

    -- * AssetKey (re-export)
    AssetKey (..),

    -- * Asset (re-export)
    Asset,
    mkAsset,
    assetKey,
    assetExpires,
    assetToken,
  )
where

import Data.ByteString.Conversion
import Data.Id
import Imports
import Wire.API.Asset.V3

--------------------------------------------------------------------------------
-- Principal

-- | A principal is an authenticated entity that can upload (and thus own)
-- and / or download assets. Different principals may be subject to
-- different restrictions on the API.
data Principal
  = UserPrincipal UserId
  | BotPrincipal BotId
  | ProviderPrincipal ProviderId
  deriving (Eq, Show)

instance ToByteString Principal where
  builder (UserPrincipal u) = builder u
  builder (BotPrincipal b) = builder b
  builder (ProviderPrincipal p) = builder p
