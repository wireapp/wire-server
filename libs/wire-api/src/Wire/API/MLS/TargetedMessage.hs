-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.MLS.TargetedMessage
  ( PersistentTargetedMessage (..),
    TargetedMessageBatch (..),
    TargetedMessageWireFormat (..),
  )
where

import Data.Binary.Get (getWord16be)
import Data.Binary.Put (putWord16be)
import Data.OpenApi qualified as S
import Imports
import Wire.API.MLS.Commit (HPKECiphertext)
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.LeafNode
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation

-- | The private wire format assigned to persistent targeted messages.
data TargetedMessageWireFormat = TargetedMessageWireFormat
  deriving stock (Eq, Show, Generic)

instance ParseMLS TargetedMessageWireFormat where
  parseMLS = do
    wireFormat <- getWord16be
    if wireFormat == 0xf001
      then pure TargetedMessageWireFormat
      else fail $ "unsupported targeted message wire format: " <> show wireFormat

instance SerialiseMLS TargetedMessageWireFormat where
  serialiseMLS _ = putWord16be 0xf001

data PersistentTargetedMessage = PersistentTargetedMessage
  { protocolVersion :: ProtocolVersion,
    wireFormat :: TargetedMessageWireFormat,
    counter :: Word32,
    sender :: LeafIndex,
    recipient :: LeafIndex,
    -- | Qualified client identity encoded as it appears in the MLS credential.
    recipientId :: ByteString,
    epoch :: Epoch,
    groupId :: GroupId,
    payload :: HPKECiphertext,
    signature :: ByteString
  }
  deriving stock (Eq, Show, Generic)

instance ParseMLS PersistentTargetedMessage where
  parseMLS =
    PersistentTargetedMessage
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLSBytes @VarInt
      <*> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLSBytes @VarInt

instance SerialiseMLS PersistentTargetedMessage where
  serialiseMLS msg = do
    serialiseMLS msg.protocolVersion
    serialiseMLS msg.wireFormat
    serialiseMLS msg.counter
    serialiseMLS msg.sender
    serialiseMLS msg.recipient
    serialiseMLSBytes @VarInt msg.recipientId
    serialiseMLS msg.epoch
    serialiseMLS msg.groupId
    serialiseMLS msg.payload
    serialiseMLSBytes @VarInt msg.signature

instance S.ToSchema PersistentTargetedMessage where
  declareNamedSchema _ = pure (mlsSwagger "PersistentTargetedMessage")

newtype TargetedMessageBatch = TargetedMessageBatch
  { messages :: [RawMLS PersistentTargetedMessage]
  }
  deriving stock (Eq, Show, Generic)

instance ParseMLS TargetedMessageBatch where
  parseMLS = TargetedMessageBatch <$> parseMLSStream parseMLS

instance SerialiseMLS TargetedMessageBatch where
  serialiseMLS = traverse_ serialiseMLS . (.messages)

instance S.ToSchema TargetedMessageBatch where
  declareNamedSchema _ = pure (mlsSwagger "TargetedMessageBatch")
