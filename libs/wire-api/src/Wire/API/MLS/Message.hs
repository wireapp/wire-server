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
    KnownFormatTag (..),
    UnreachableUserList (..),
    verifyMessageSignature,
    mkSignedMessage,
  )
where

import Control.Lens ((?~))
import Crypto.PubKey.Ed25519
import qualified Data.Aeson as A
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteArray as BA
import Data.Id
import Data.Json.Util
import Data.Kind
import Data.Qualified
import Data.Schema
import Data.Singletons.TH
import qualified Data.Swagger as S
import Imports
import Test.QuickCheck hiding (label)
import Wire.API.Event.Conversation
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.Arbitrary (GenericUniform (..))

data WireFormatTag = MLSPlainText | MLSCipherText
  deriving (Bounded, Enum, Eq, Show)

$(genSingletons [''WireFormatTag])

instance ParseMLS WireFormatTag where
  parseMLS = parseMLSEnum @Word8 "wire format"

data family MessageExtraFields (tag :: WireFormatTag) :: Type

data instance MessageExtraFields 'MLSPlainText = MessageExtraFields
  { msgSignature :: ByteString,
    msgConfirmation :: Maybe ByteString,
    msgMembership :: Maybe ByteString
  }
  deriving (Generic)
  deriving (Arbitrary) via (GenericUniform (MessageExtraFields 'MLSPlainText))

instance ParseMLS (MessageExtraFields 'MLSPlainText) where
  parseMLS =
    MessageExtraFields
      <$> label "msgSignature" (parseMLSBytes @Word16)
      <*> label "msgConfirmation" (parseMLSOptional (parseMLSBytes @Word8))
      <*> label "msgMembership" (parseMLSOptional (parseMLSBytes @Word8))

instance SerialiseMLS (MessageExtraFields 'MLSPlainText) where
  serialiseMLS (MessageExtraFields sig mconf mmemb) = do
    serialiseMLSBytes @Word16 sig
    serialiseMLSOptional (serialiseMLSBytes @Word8) mconf
    serialiseMLSOptional (serialiseMLSBytes @Word8) mmemb

data instance MessageExtraFields 'MLSCipherText = NoExtraFields

instance ParseMLS (MessageExtraFields 'MLSCipherText) where
  parseMLS = pure NoExtraFields

deriving instance Eq (MessageExtraFields 'MLSPlainText)

deriving instance Eq (MessageExtraFields 'MLSCipherText)

deriving instance Show (MessageExtraFields 'MLSPlainText)

deriving instance Show (MessageExtraFields 'MLSCipherText)

data Message (tag :: WireFormatTag) = Message
  { msgTBS :: RawMLS (MessageTBS tag),
    msgExtraFields :: MessageExtraFields tag
  }

deriving instance Eq (Message 'MLSPlainText)

deriving instance Eq (Message 'MLSCipherText)

deriving instance Show (Message 'MLSPlainText)

deriving instance Show (Message 'MLSCipherText)

instance ParseMLS (Message 'MLSPlainText) where
  parseMLS = Message <$> label "tbs" parseMLS <*> label "MessageExtraFields" parseMLS

instance SerialiseMLS (Message 'MLSPlainText) where
  serialiseMLS (Message msgTBS msgExtraFields) = do
    putByteString (rmRaw msgTBS)
    serialiseMLS msgExtraFields

instance ParseMLS (Message 'MLSCipherText) where
  parseMLS = Message <$> parseMLS <*> parseMLS

-- | This corresponds to the format byte at the beginning of a message.
-- It does not convey any information, but it needs to be present in
-- order for signature verification to work.
data KnownFormatTag (tag :: WireFormatTag) = KnownFormatTag

instance ParseMLS (KnownFormatTag tag) where
  parseMLS = parseMLS @WireFormatTag $> KnownFormatTag

instance SerialiseMLS (KnownFormatTag 'MLSPlainText) where
  serialiseMLS _ = put (fromMLSEnum @Word8 MLSPlainText)

instance SerialiseMLS (KnownFormatTag 'MLSCipherText) where
  serialiseMLS _ = put (fromMLSEnum @Word8 MLSCipherText)

deriving instance Eq (KnownFormatTag 'MLSPlainText)

deriving instance Eq (KnownFormatTag 'MLSCipherText)

deriving instance Show (KnownFormatTag 'MLSPlainText)

deriving instance Show (KnownFormatTag 'MLSCipherText)

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

instance SerialiseMLS (MessageTBS 'MLSPlainText) where
  serialiseMLS (MessageTBS f g e d s p) = do
    serialiseMLS f
    serialiseMLS g
    serialiseMLS e
    serialiseMLS s
    serialiseMLSBytes @Word32 d
    serialiseMLS p

deriving instance Eq (MessageTBS 'MLSPlainText)

deriving instance Eq (MessageTBS 'MLSCipherText)

deriving instance Show (MessageTBS 'MLSPlainText)

deriving instance Show (MessageTBS 'MLSCipherText)

data SomeMessage where
  SomeMessage :: Sing tag -> Message tag -> SomeMessage

instance S.ToSchema SomeMessage where
  declareNamedSchema _ = pure (mlsSwagger "MLSMessage")

instance ParseMLS SomeMessage where
  parseMLS =
    lookAhead parseMLS >>= \case
      MLSPlainText -> SomeMessage SMLSPlainText <$> parseMLS
      MLSCipherText -> SomeMessage SMLSCipherText <$> parseMLS

data family Sender (tag :: WireFormatTag) :: Type

data instance Sender 'MLSCipherText = EncryptedSender {esData :: ByteString}
  deriving (Eq, Show)

instance ParseMLS (Sender 'MLSCipherText) where
  parseMLS = EncryptedSender <$> parseMLSBytes @Word8

data SenderTag = MemberSenderTag | PreconfiguredSenderTag | NewMemberSenderTag
  deriving (Bounded, Enum, Show, Eq)

instance ParseMLS SenderTag where
  parseMLS = parseMLSEnum @Word8 "sender type"

instance SerialiseMLS SenderTag where
  serialiseMLS = serialiseMLSEnum @Word8

-- NOTE: according to the spec, the preconfigured sender case contains a
-- bytestring, not a u32. However, as of 2022-08-02, the openmls fork used by
-- the clients is using a u32 here.
data instance Sender 'MLSPlainText
  = MemberSender KeyPackageRef
  | PreconfiguredSender Word32
  | NewMemberSender
  deriving (Eq, Show, Generic)

instance ParseMLS (Sender 'MLSPlainText) where
  parseMLS =
    parseMLS >>= \case
      MemberSenderTag -> MemberSender <$> parseMLS
      PreconfiguredSenderTag -> PreconfiguredSender <$> get
      NewMemberSenderTag -> pure NewMemberSender

instance SerialiseMLS (Sender 'MLSPlainText) where
  serialiseMLS (MemberSender r) = do
    serialiseMLS MemberSenderTag
    serialiseMLS r
  serialiseMLS (PreconfiguredSender x) = do
    serialiseMLS PreconfiguredSenderTag
    put x
  serialiseMLS NewMemberSender = serialiseMLS NewMemberSenderTag

data family MessagePayload (tag :: WireFormatTag) :: Type

deriving instance Eq (MessagePayload 'MLSPlainText)

deriving instance Eq (MessagePayload 'MLSCipherText)

deriving instance Show (MessagePayload 'MLSPlainText)

deriving instance Show (MessagePayload 'MLSCipherText)

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

instance SerialiseMLS ContentType where
  serialiseMLS = serialiseMLSEnum @Word8

instance SerialiseMLS (MessagePayload 'MLSPlainText) where
  serialiseMLS (ProposalMessage raw) = do
    serialiseMLS ProposalMessageTag
    putByteString (rmRaw raw)
  -- We do not need to serialise Commit and Application messages,
  -- so the next case is left as a stub
  serialiseMLS _ = pure ()

newtype UnreachableUserList = UnreachableUserList {unreachableUsers :: [Qualified UserId]}
  deriving stock (Eq, Show)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema UnreachableUserList
  deriving newtype (Semigroup, Monoid)

instance ToSchema UnreachableUserList where
  schema =
    named "UnreachableUserList" $
      UnreachableUserList
        <$> unreachableUsers
          .= array schema

data MLSMessageSendingStatus = MLSMessageSendingStatus
  { mmssEvents :: [Event],
    mmssTime :: UTCTimeMillis,
    mmssUnreachableUserList :: UnreachableUserList
  }
  deriving (Eq, Show)
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
        <*> mmssUnreachableUserList
          .= fieldWithDocModifier
            "failed_to_send"
            (description ?~ "List of federated users who could not be reached and did not receive the message")
            schema

verifyMessageSignature :: CipherSuiteTag -> Message 'MLSPlainText -> ByteString -> Bool
verifyMessageSignature cs msg pubkey =
  csVerifySignature cs pubkey (rmRaw (msgTBS msg)) (msgSignature (msgExtraFields msg))

mkSignedMessage ::
  SecretKey ->
  PublicKey ->
  GroupId ->
  Epoch ->
  MessagePayload 'MLSPlainText ->
  Message 'MLSPlainText
mkSignedMessage priv pub gid epoch payload =
  let tbs =
        mkRawMLS $
          MessageTBS
            { tbsMsgFormat = KnownFormatTag,
              tbsMsgGroupId = gid,
              tbsMsgEpoch = epoch,
              tbsMsgAuthData = mempty,
              tbsMsgSender = PreconfiguredSender 0,
              tbsMsgPayload = payload
            }
      sig = BA.convert $ sign priv pub (rmRaw tbs)
   in Message tbs (MessageExtraFields sig Nothing Nothing)
