{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.ConversationStore.Migration.Types where

import Data.Bits
import Data.Id
import Data.UUID qualified as UUID
import Imports
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.LeafNode
import Wire.ConversationStore.MLS.Types
import Wire.MigrationLock
import Wire.StoredConversation

data ConvMLSDetails = ConvMLSDetails
  { groupInfoData :: GroupInfoData,
    clientMap :: ClientMap LeafIndex,
    indexMap :: IndexMap
  }

data AllSubConvData = AllSubConvData
  { subConv :: SubConversation,
    groupInfoData :: Maybe GroupInfoData
  }

data AllConvData = AllConvData
  { conv :: StoredConversation,
    mlsDetails :: Maybe ConvMLSDetails,
    subConvs :: [AllSubConvData]
  }

instance MigrationLockable ConvId where
  lockKey = hashUUID
  lockScope = "conv"

instance MigrationLockable UserId where
  lockKey = hashUUID
  lockScope = "user"

hashUUID :: Id a -> Int64
hashUUID (toUUID -> uuid) =
  let (w1, w2) = UUID.toWords64 uuid
      mixed = w1 `xor` (w2 `shiftR` 32) `xor` (w2 `shiftL` 32)
   in fromIntegral mixed
