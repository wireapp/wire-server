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
  ( GroupContext (..),
    GroupInfo (..),
    GroupInfoData (..),
    GroupInfoTBS (..),
  )
where

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy qualified as LBS
import Data.OpenApi qualified as S
import GHC.Records
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Epoch
import Wire.API.MLS.Extension
import Wire.API.MLS.Group
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-8.1-2
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

instance SerialiseMLS GroupContext where
  serialiseMLS gc = do
    serialiseMLS gc.protocolVersion
    serialiseMLS gc.cipherSuite
    serialiseMLS gc.groupId
    serialiseMLS gc.epoch
    serialiseMLSBytes @VarInt gc.treeHash
    serialiseMLSBytes @VarInt gc.confirmedTranscriptHash
    serialiseMLSVector @VarInt serialiseMLS gc.extensions

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-12.4.3-7
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

instance SerialiseMLS GroupInfoTBS where
  serialiseMLS tbs = do
    serialiseMLS tbs.groupContext
    serialiseMLSVector @VarInt serialiseMLS tbs.extensions
    serialiseMLSBytes @VarInt tbs.confirmationTag
    serialiseMLS tbs.signer

-- | https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol-20/draft-ietf-mls-protocol.html#section-12.4.3-2
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

instance SerialiseMLS GroupInfo where
  serialiseMLS gi = do
    serialiseMLS gi.tbs
    serialiseMLSBytes @VarInt gi.signature_

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
