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

module Wire.API.MLS.GroupInfoBundle where

import Control.Lens (view, (.~))
import Data.ProtoLens (Message (defMessage))
import Imports
import qualified Proto.Mls
import qualified Proto.Mls_Fields as Proto.Mls
import Test.QuickCheck
import Wire.API.ConverProtoLens
import qualified Wire.API.ConverProtoLens as CP
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

data GroupInfoType = GroupInfoTypePublicGroupState | UnencryptedGroupInfo | JweEncryptedGroupInfo
  deriving stock (Eq, Show, Generic, Enum, Bounded)
  deriving (Arbitrary) via (GenericUniform GroupInfoType)

instance ConvertProtoLens Proto.Mls.GroupInfoType GroupInfoType where
  fromProtolens Proto.Mls.PUBLIC_GROUP_STATE = pure GroupInfoTypePublicGroupState
  fromProtolens Proto.Mls.GROUP_INFO = pure UnencryptedGroupInfo
  fromProtolens Proto.Mls.GROUP_INFO_JWE = pure JweEncryptedGroupInfo

  toProtolens GroupInfoTypePublicGroupState = Proto.Mls.PUBLIC_GROUP_STATE
  toProtolens UnencryptedGroupInfo = Proto.Mls.GROUP_INFO
  toProtolens JweEncryptedGroupInfo = Proto.Mls.GROUP_INFO_JWE

data RatchetTreeType = TreeFull | TreeDelta | TreeByRef
  deriving stock (Eq, Show, Generic, Bounded, Enum)
  deriving (Arbitrary) via (GenericUniform RatchetTreeType)

instance ConvertProtoLens Proto.Mls.RatchetTreeType RatchetTreeType where
  fromProtolens Proto.Mls.FULL = pure TreeFull
  fromProtolens Proto.Mls.DELTA = pure TreeDelta
  fromProtolens Proto.Mls.REFERENCE = pure TreeByRef

  toProtolens TreeFull = Proto.Mls.FULL
  toProtolens TreeDelta = Proto.Mls.DELTA
  toProtolens TreeByRef = Proto.Mls.REFERENCE

data GroupInfoBundle = GroupInfoBundle
  { gipGroupInfoType :: GroupInfoType,
    gipRatchetTreeType :: RatchetTreeType,
    gipGroupState :: RawMLS PublicGroupState
  }
  deriving stock (Eq, Show, Generic)

instance ConvertProtoLens Proto.Mls.GroupInfoBundle GroupInfoBundle where
  fromProtolens protoBundle =
    CP.label "GroupInfoBundle" $
      GroupInfoBundle
        <$> CP.label "field group_info_type" (fromProtolens (view Proto.Mls.groupInfoType protoBundle))
        <*> CP.label "field ratchet_tree_type" (fromProtolens (view Proto.Mls.ratchetTreeType protoBundle))
        <*> CP.label "field group_info" (decodeMLS' (view Proto.Mls.groupInfo protoBundle))
  toProtolens bundle =
    let encryptionType = toProtolens (gipGroupInfoType bundle)
        treeType = toProtolens (gipRatchetTreeType bundle)
     in ( defMessage
            & Proto.Mls.groupInfoType .~ encryptionType
            & Proto.Mls.ratchetTreeType .~ treeType
            & Proto.Mls.groupInfo .~ rmRaw (gipGroupState bundle)
        )

instance Arbitrary GroupInfoBundle where
  arbitrary =
    GroupInfoBundle
      <$> arbitrary
      <*> arbitrary
      <*> (mkRawMLS <$> arbitrary)

instance ParseMLS GroupInfoBundle where
  parseMLS =
    GroupInfoBundle
      <$> parseMLSEnum @Word8 "GroupInfoTypeEnum"
      <*> parseMLSEnum @Word8 "RatchetTreeEnum"
      <*> parseMLS

instance SerialiseMLS GroupInfoBundle where
  serialiseMLS (GroupInfoBundle e t pgs) = do
    serialiseMLSEnum @Word8 e
    serialiseMLSEnum @Word8 t
    serialiseMLS pgs
