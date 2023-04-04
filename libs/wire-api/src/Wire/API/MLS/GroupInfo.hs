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

module Wire.API.MLS.GroupInfo
  ( GroupInfo (..),
    GroupInfoData (..),
  )
where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Swagger as S
import GHC.Records
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Epoch
import Wire.API.MLS.Extension
import Wire.API.MLS.Group
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

data GroupContext = GroupContext
  { protocolVersion :: ProtocolVersion,
    cipherSuite :: CipherSuite,
    groupId :: GroupId,
    epoch :: Epoch,
    treeHash :: ByteString,
    confirmedTranscriptHash :: ByteString,
    extensions :: [Extension]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GroupContext)

instance ParseMLS GroupContext where
  parseMLS =
    GroupContext
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLSBytes @VarInt
      <*> parseMLSBytes @VarInt
      <*> parseMLSVector @VarInt parseMLS

data GroupInfoTBS = GroupInfoTBS
  { groupContext :: GroupContext,
    extensions :: [Extension],
    confirmationTag :: ByteString,
    signer :: Word32
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GroupInfoTBS)

instance ParseMLS GroupInfoTBS where
  parseMLS =
    GroupInfoTBS
      <$> parseMLS
      <*> parseMLSVector @VarInt parseMLS
      <*> parseMLSBytes @VarInt
      <*> parseMLS

data GroupInfo = GroupInfo
  { tbs :: GroupInfoTBS,
    signature_ :: ByteString
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GroupInfo)

instance ParseMLS GroupInfo where
  parseMLS =
    GroupInfo
      <$> parseMLS
      <*> parseMLSBytes @VarInt

instance HasField "groupContext" GroupInfo GroupContext where
  getField = (.tbs.groupContext)

instance HasField "extensions" GroupInfo [Extension] where
  getField = (.tbs.extensions)

instance HasField "confirmationTag" GroupInfo ByteString where
  getField = (.tbs.confirmationTag)

instance HasField "signer" GroupInfo Word32 where
  getField = (.tbs.signer)

newtype GroupInfoData = GroupInfoData {unGroupInfoData :: ByteString}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Arbitrary)

instance ParseMLS GroupInfoData where
  parseMLS = GroupInfoData . LBS.toStrict <$> getRemainingLazyByteString

instance SerialiseMLS GroupInfoData where
  serialiseMLS (GroupInfoData bs) = putByteString bs

instance S.ToSchema GroupInfoData where
  declareNamedSchema _ = pure (mlsSwagger "GroupInfoData")
