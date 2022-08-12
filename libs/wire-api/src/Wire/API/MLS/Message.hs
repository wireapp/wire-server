{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

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
    msgGroupId,
    msgEpoch,
    msgSender,
    msgPayload,
    MessageTBS (..),
    MessageExtraFields (..),
    WireFormatTag (..),
    SWireFormatTag (..),
    SomeMessage (..),
    ContentType (..),
    MessagePayload (..),
    Sender (..),
    MLSPlainTextSym0,
    MLSCipherTextSym0,
    MLSMessageSendingStatus (..),
    verifyMessageSignature,
  )
where

import Control.Lens ((?~))
import qualified Data.Aeson as A
import Data.Binary
import Data.Binary.Get
import Data.Json.Util
import Data.Schema
import Data.Singletons.TH
import qualified Data.Swagger as S
import Imports
import Wire.API.Event.Conversation
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation

data WireFormatTag = MLSPlainText | MLSCipherText
  deriving (Bounded, Enum, Eq, Show)

$(genSingletons [''WireFormatTag])

instance ParseMLS WireFormatTag where
  parseMLS = parseMLSEnum @Word8 "wire format"

data family MessageExtraFields (tag :: WireFormatTag) :: *

data instance MessageExtraFields 'MLSPlainText = MessageExtraFields
  { msgSignature :: ByteString,
    msgConfirmation :: Maybe ByteString,
    msgMembership :: Maybe ByteString
  }

instance ParseMLS (MessageExtraFields 'MLSPlainText) where
  parseMLS =
    MessageExtraFields
      <$> parseMLSBytes @Word16
      <*> parseMLSOptional (parseMLSBytes @Word8)
      <*> parseMLSOptional (parseMLSBytes @Word8)

data instance MessageExtraFields 'MLSCipherText = NoExtraFields

instance ParseMLS (MessageExtraFields 'MLSCipherText) where
  parseMLS = pure NoExtraFields

data Message (tag :: WireFormatTag) = Message
  { msgTBS :: RawMLS (MessageTBS tag),
    msgExtraFields :: MessageExtraFields tag
  }

instance ParseMLS (Message 'MLSPlainText) where
  parseMLS = Message <$> parseMLS <*> parseMLS

instance ParseMLS (Message 'MLSCipherText) where
  parseMLS = Message <$> parseMLS <*> parseMLS

-- | This corresponds to the format byte at the beginning of a message.
-- It does not convey any information, but it needs to be present in
-- order for signature verification to work.
data KnownFormatTag (tag :: WireFormatTag) = KnownFormatTag

instance ParseMLS (KnownFormatTag tag) where
  parseMLS = parseMLS @WireFormatTag $> KnownFormatTag

data MessageTBS (tag :: WireFormatTag) = MessageTBS
  { tbsMsgFormat :: KnownFormatTag tag,
    tbsMsgGroupId :: GroupId,
    tbsMsgEpoch :: Epoch,
    tbsMsgAuthData :: ByteString,
    tbsMsgSender :: Sender tag,
    tbsMsgPayload :: MessagePayload tag
  }

msgGroupId :: Message tag -> GroupId
msgGroupId = tbsMsgGroupId . rmValue . msgTBS

msgEpoch :: Message tag -> Epoch
msgEpoch = tbsMsgEpoch . rmValue . msgTBS

msgSender :: Message tag -> Sender tag
msgSender = tbsMsgSender . rmValue . msgTBS

msgPayload :: Message tag -> MessagePayload tag
msgPayload = tbsMsgPayload . rmValue . msgTBS

instance ParseMLS (MessageTBS 'MLSPlainText) where
  parseMLS = do
    f <- parseMLS
    g <- parseMLS
    e <- parseMLS
    s <- parseMLS
    d <- parseMLSBytes @Word32
    MessageTBS f g e d s <$> parseMLS

instance ParseMLS (MessageTBS 'MLSCipherText) where
  parseMLS = do
    f <- parseMLS
    g <- parseMLS
    e <- parseMLS
    ct <- parseMLS
    d <- parseMLSBytes @Word32
    s <- parseMLS
    p <- parseMLSBytes @Word32
    pure $ MessageTBS f g e d s (CipherText ct p)

data SomeMessage where
  SomeMessage :: Sing tag -> Message tag -> SomeMessage

instance S.ToSchema SomeMessage where
  declareNamedSchema _ = pure (mlsSwagger "MLSMessage")

instance ParseMLS SomeMessage where
  parseMLS =
    lookAhead parseMLS >>= \case
      MLSPlainText -> SomeMessage SMLSPlainText <$> parseMLS
      MLSCipherText -> SomeMessage SMLSCipherText <$> parseMLS

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

data ContentType
  = ApplicationMessageTag
  | ProposalMessageTag
  | CommitMessageTag
  deriving (Bounded, Enum, Eq, Show)

instance ParseMLS ContentType where
  parseMLS = parseMLSEnum @Word8 "content type"

data instance MessagePayload 'MLSPlainText
  = ApplicationMessage ByteString
  | ProposalMessage (RawMLS Proposal)
  | CommitMessage Commit

instance ParseMLS (MessagePayload 'MLSPlainText) where
  parseMLS =
    parseMLS >>= \case
      ApplicationMessageTag -> ApplicationMessage <$> parseMLSBytes @Word32
      ProposalMessageTag -> ProposalMessage <$> parseMLS
      CommitMessageTag -> CommitMessage <$> parseMLS

data MLSMessageSendingStatus = MLSMessageSendingStatus
  { mmssEvents :: [Event],
    mmssTime :: UTCTimeMillis
  }
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema MLSMessageSendingStatus

instance ToSchema MLSMessageSendingStatus where
  schema =
    object "MLSMessageSendingStatus" $
      MLSMessageSendingStatus
        <$> mmssEvents
          .= fieldWithDocModifier
            "events"
            (description ?~ "A list of events caused by sending the message.")
            (array schema)
        <*> mmssTime
          .= fieldWithDocModifier
            "time"
            (description ?~ "The time of sending the message.")
            schema

verifyMessageSignature :: CipherSuiteTag -> Message 'MLSPlainText -> ByteString -> Bool
verifyMessageSignature cs msg pubkey =
  csVerifySignature cs pubkey (rmRaw (msgTBS msg)) (msgSignature (msgExtraFields msg))
