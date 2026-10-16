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

module Wire.API.MLS.TransientMessage
  ( TransientMessage (..),
    TransientMessageTBS (..),
    TransientMessageAad (..),
    TransientMessageWireFormat (..),
    transientMessageAad,
    transientMessageWireFormat,
  )
where

import Data.Binary.Get
import Data.Binary.Put
import Data.OpenApi qualified as S
import Imports
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.LeafNode (LeafIndex)
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation
import Wire.Arbitrary

-- | Private wire format reserved for a non-persisted broadcast MLS message.
-- | The only supported transient-message wire format. Keeping this as a
-- nullary type prevents callers from constructing a message with another tag.
data TransientMessageWireFormat = TransientMessageWireFormat
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TransientMessageWireFormat)

transientMessageWireFormat :: TransientMessageWireFormat
transientMessageWireFormat = TransientMessageWireFormat

instance ParseMLS TransientMessageWireFormat where
  parseMLS = do
    wireFormat <- getWord16be
    unless (wireFormat == 0xf000) $
      fail $
        "unexpected transient message wire format: " <> show wireFormat
    pure TransientMessageWireFormat

instance SerialiseMLS TransientMessageWireFormat where
  serialiseMLS _ = putWord16be 0xf000

-- | Authenticated data used to encrypt a transient message. This is not a
-- nested field in the transmitted message; Core Crypto serializes it directly
-- as protocol version, wire format, group context, sender, and counter.
data TransientMessageAad = TransientMessageAad
  { protocolVersion :: ProtocolVersion,
    wireFormat :: TransientMessageWireFormat,
    groupContext :: GroupContext,
    sender :: LeafIndex,
    counter :: Word32
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TransientMessageAad)

instance ParseMLS TransientMessageAad where
  parseMLS = do
    protocolVersion <- parseMLS
    wireFormat <- parseMLS
    groupContext <- parseMLS
    sender <- parseMLS
    counter <- parseMLS
    pure $ TransientMessageAad protocolVersion wireFormat groupContext sender counter

instance SerialiseMLS TransientMessageAad where
  serialiseMLS aad = do
    serialiseMLS aad.protocolVersion
    serialiseMLS aad.wireFormat
    serialiseMLS aad.groupContext
    serialiseMLS aad.sender
    serialiseMLS aad.counter

-- | The transient-message body, serialized after the protocol version and
-- 0xf000 wire-format discriminator.
data TransientMessageTBS = TransientMessageTBS
  { groupContext :: GroupContext,
    sender :: LeafIndex,
    counter :: Word32,
    payload :: ByteString
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TransientMessageTBS)

instance ParseMLS TransientMessageTBS where
  parseMLS = TransientMessageTBS <$> parseMLS <*> parseMLS <*> parseMLS <*> parseMLSBytes @VarInt

instance SerialiseMLS TransientMessageTBS where
  serialiseMLS tbs = do
    serialiseMLS tbs.groupContext
    serialiseMLS tbs.sender
    serialiseMLS tbs.counter
    serialiseMLSBytes @VarInt tbs.payload

data TransientMessage = TransientMessage
  { protocolVersion :: ProtocolVersion,
    wireFormat :: TransientMessageWireFormat,
    inner :: TransientMessageTBS,
    signature :: ByteString
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TransientMessage)

instance ParseMLS TransientMessage where
  parseMLS = do
    protocolVersion <- parseMLS
    wireFormat <- parseMLS
    inner <- parseMLS
    signature <- parseMLSBytes @VarInt
    pure $ TransientMessage protocolVersion wireFormat inner signature

instance SerialiseMLS TransientMessage where
  serialiseMLS message = do
    serialiseMLS message.protocolVersion
    serialiseMLS message.wireFormat
    serialiseMLS message.inner
    serialiseMLSBytes @VarInt message.signature

transientMessageAad :: TransientMessage -> TransientMessageAad
transientMessageAad message =
  TransientMessageAad
    { protocolVersion = message.protocolVersion,
      wireFormat = message.wireFormat,
      groupContext = message.inner.groupContext,
      sender = message.inner.sender,
      counter = message.inner.counter
    }

instance S.ToSchema TransientMessage where
  declareNamedSchema _ = pure (mlsSwagger "TransientMessage")
