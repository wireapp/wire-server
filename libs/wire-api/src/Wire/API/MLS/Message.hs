{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

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

module Wire.API.MLS.Message
  ( Message (..),
    SomeMessage (..),
    MessagePayload (..),
    MessagePayloadTBS (..),
    Sender (..),
    MLSPlainTextSym0,
    MLSCipherTextSym0,
  )
where

import Data.Binary
import Data.Singletons.TH
import Imports
import Wire.API.MLS.Commit
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

data WireFormatTag = MLSPlainText | MLSCipherText
  deriving (Bounded, Enum, Eq, Show)

$(genSingletons [''WireFormatTag])

instance ParseMLS WireFormatTag where
  parseMLS = parseMLSEnum @Word8 "wire format"

data Message (tag :: WireFormatTag) = Message
  { msgGroupId :: GroupId,
    msgEpoch :: Word64,
    msgAuthData :: ByteString,
    msgSender :: Sender tag,
    msgPayload :: MessagePayload tag
  }

instance ParseMLS (Message 'MLSPlainText) where
  parseMLS = do
    g <- parseMLS
    e <- parseMLS
    s <- parseMLS
    d <- parseMLSBytes @Word32
    p <- parseMLS
    pure (Message g e d s p)

instance ParseMLS (Message 'MLSCipherText) where
  parseMLS = do
    g <- parseMLS
    e <- parseMLS
    ct <- parseMLS
    d <- parseMLSBytes @Word32
    s <- parseMLS
    p <- parseMLSBytes @Word32
    pure $ Message g e d s (CipherText ct p)

data SomeMessage where
  SomeMessage :: Message tag -> SomeMessage

instance ParseMLS SomeMessage where
  parseMLS =
    parseMLS >>= \case
      MLSPlainText -> SomeMessage @'MLSPlainText <$> parseMLS
      MLSCipherText -> SomeMessage @'MLSCipherText <$> parseMLS

data family Sender (tag :: WireFormatTag) :: *

data instance Sender 'MLSCipherText = EncryptedSender {esData :: ByteString}

instance ParseMLS (Sender 'MLSCipherText) where
  parseMLS = EncryptedSender <$> parseMLSBytes @Word8

data SenderTag = MemberSenderTag | PreconfiguredSenderTag | NewMemberSenderTag
  deriving (Bounded, Enum, Show, Eq)

instance ParseMLS SenderTag where
  parseMLS = parseMLSEnum @Word8 "sender type"

data instance Sender 'MLSPlainText
  = MemberSender KeyPackageRef
  | PreconfiguredSender ByteString
  | NewMemberSender

instance ParseMLS (Sender 'MLSPlainText) where
  parseMLS =
    parseMLS >>= \case
      MemberSenderTag -> MemberSender <$> parseMLS
      PreconfiguredSenderTag -> PreconfiguredSender <$> parseMLSBytes @Word8
      NewMemberSenderTag -> pure NewMemberSender

data family MessagePayload (tag :: WireFormatTag) :: *

data instance MessagePayload 'MLSCipherText = CipherText
  { msgContentType :: Word8,
    msgCipherText :: ByteString
  }

data instance MessagePayload 'MLSPlainText = MessagePayload
  { msgTBS :: MessagePayloadTBS,
    msgSignature :: ByteString,
    msgConfirmation :: Maybe ByteString,
    msgMembership :: Maybe ByteString
  }

instance ParseMLS (MessagePayload 'MLSPlainText) where
  parseMLS =
    MessagePayload
      <$> parseMLS
        <*> parseMLSBytes @Word16
        <*> parseMLSOptional (parseMLSBytes @Word8)
        <*> parseMLSOptional (parseMLSBytes @Word8)

data MessagePayloadTBS
  = ApplicationMessage ByteString
  | ProposalMessage Proposal
  | CommitMessage Commit

data ContentType
  = ApplicationMessageTag
  | ProposalMessageTag
  | CommitMessageTag
  deriving (Bounded, Enum, Eq, Show)

instance ParseMLS ContentType where
  parseMLS = parseMLSEnum @Word8 "content type"

instance ParseMLS MessagePayloadTBS where
  parseMLS =
    parseMLS >>= \case
      ApplicationMessageTag -> ApplicationMessage <$> parseMLSBytes @Word32
      ProposalMessageTag -> ProposalMessage <$> parseMLS
      CommitMessageTag -> CommitMessage <$> parseMLS
