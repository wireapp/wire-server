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
    TransientTargetedMessage,
    TargetedMessageBatch (..),
  )
where

import Data.Binary.Get (Get, getWord16be)
import Data.Binary.Put (Put, putWord16be)
import Data.OpenApi qualified as S
import Imports
import Wire.API.MLS.Commit (HPKECiphertext)
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.LeafNode
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation

persistentTargetedMessageWireFormat :: Word16
persistentTargetedMessageWireFormat = 0xf001

transientTargetedMessageWireFormat :: Word16
transientTargetedMessageWireFormat = 0xf002

data PersistentTargetedMessage = PersistentTargetedMessage
  { protocolVersion :: ProtocolVersion,
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
  parseMLS = parseTargetedMessageWithWireFormat persistentTargetedMessageWireFormat

instance SerialiseMLS PersistentTargetedMessage where
  serialiseMLS = serialiseTargetedMessageWithWireFormat persistentTargetedMessageWireFormat

instance S.ToSchema PersistentTargetedMessage where
  declareNamedSchema _ = pure (mlsSwagger "PersistentTargetedMessage")

newtype TransientTargetedMessage = TransientTargetedMessage PersistentTargetedMessage
  deriving stock (Eq, Show, Generic)

instance ParseMLS TransientTargetedMessage where
  parseMLS = TransientTargetedMessage <$> parseTargetedMessageWithWireFormat transientTargetedMessageWireFormat

instance SerialiseMLS TransientTargetedMessage where
  serialiseMLS (TransientTargetedMessage message) = serialiseTargetedMessageWithWireFormat transientTargetedMessageWireFormat message

instance S.ToSchema TransientTargetedMessage where
  declareNamedSchema _ = pure (mlsSwagger "TransientTargetedMessage")

parseTargetedMessageWithWireFormat :: Word16 -> Get PersistentTargetedMessage
parseTargetedMessageWithWireFormat expectedWireFormat = do
  protocolVersion <- parseMLS
  wireFormat <- getWord16be
  unless (wireFormat == expectedWireFormat) $
    fail $
      "unexpected targeted message wire format: " <> show wireFormat
  PersistentTargetedMessage protocolVersion
    <$> parseMLS
    <*> parseMLS
    <*> parseMLS
    <*> parseMLSBytes @VarInt
    <*> parseMLS
    <*> parseMLS
    <*> parseMLS
    <*> parseMLSBytes @VarInt

serialiseTargetedMessageWithWireFormat :: Word16 -> PersistentTargetedMessage -> Put
serialiseTargetedMessageWithWireFormat wireFormat msg = do
  serialiseMLS msg.protocolVersion
  putWord16be wireFormat
  serialiseMLS msg.counter
  serialiseMLS msg.sender
  serialiseMLS msg.recipient
  serialiseMLSBytes @VarInt msg.recipientId
  serialiseMLS msg.epoch
  serialiseMLS msg.groupId
  serialiseMLS msg.payload
  serialiseMLSBytes @VarInt msg.signature

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
