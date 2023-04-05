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
{-# OPTIONS_GHC -Wwarn #-}

module Wire.API.MLS.Message
  ( -- * MLS Message types
    Message (..),
    mkMessage,
    MessageContent (..),
    PublicMessage (..),
    PrivateMessage (..),
    FramedContent (..),
    FramedContentData (..),
    FramedContentDataTag (..),
    FramedContentTBS (..),
    FramedContentAuthData (..),
    Sender (..),
    UnreachableUsers (..),

    -- * Utilities
    verifyMessageSignature,
    mkSignedMessage,

    -- * Servant types
    MLSMessageSendingStatus (..),
  )
where

import Control.Lens ((?~))
import Crypto.PubKey.Ed25519
import qualified Data.Aeson as A
import Data.Binary
import qualified Data.ByteArray as BA
import Data.Id
import Data.Json.Util
import Data.Kind
import Data.Qualified
import Data.Schema
import Data.Schema hiding (tag)
import Data.Singletons.TH
import qualified Data.Swagger as S
import GHC.Records
import Imports
import Wire.API.Event.Conversation
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Commit
import Wire.API.MLS.Epoch
import Wire.API.MLS.Extension
import Wire.API.MLS.Group
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Proposal
import Wire.API.MLS.ProtocolVersion
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome
import Wire.Arbitrary

data WireFormatTag
  = WireFormatPublicTag
  | WireFormatPrivateTag
  | WireFormatWelcomeTag
  | WireFormatGroupInfoTag
  | WireFormatKeyPackageTag
  deriving (Enum, Bounded, Eq, Show)

instance ParseMLS WireFormatTag where
  parseMLS = parseMLSEnum @Word16 "wire format"

instance SerialiseMLS WireFormatTag where
  serialiseMLS = serialiseMLSEnum @Word16

data Message = Message
  { protocolVersion :: ProtocolVersion,
    content :: MessageContent
  }
  deriving (Eq, Show)

mkMessage :: MessageContent -> Message
mkMessage = Message defaultProtocolVersion

instance ParseMLS Message where
  parseMLS =
    Message
      <$> parseMLS
      <*> parseMLS

instance SerialiseMLS Message where
  serialiseMLS msg = do
    serialiseMLS msg.protocolVersion
    serialiseMLS msg.content

instance HasField "wireFormat" Message WireFormatTag where
  getField = (.content.wireFormat)

data MessageContent
  = MessagePrivate (RawMLS PrivateMessage)
  | MessagePublic PublicMessage
  | MessageWelcome (RawMLS Welcome)
  | MessageGroupInfo (RawMLS GroupInfo)
  | MessageKeyPackage (RawMLS KeyPackage)
  deriving (Eq, Show)

instance HasField "wireFormat" MessageContent WireFormatTag where
  getField (MessagePrivate _) = WireFormatPrivateTag
  getField (MessagePublic _) = WireFormatPublicTag
  getField (MessageWelcome _) = WireFormatWelcomeTag
  getField (MessageGroupInfo _) = WireFormatGroupInfoTag
  getField (MessageKeyPackage _) = WireFormatKeyPackageTag

instance ParseMLS MessageContent where
  parseMLS =
    parseMLS >>= \case
      WireFormatPrivateTag -> MessagePrivate <$> parseMLS
      WireFormatPublicTag -> MessagePublic <$> parseMLS
      WireFormatWelcomeTag -> MessageWelcome <$> parseMLS
      WireFormatGroupInfoTag -> MessageGroupInfo <$> parseMLS
      WireFormatKeyPackageTag -> MessageKeyPackage <$> parseMLS

instance SerialiseMLS MessageContent where
  serialiseMLS (MessagePrivate msg) = do
    serialiseMLS WireFormatPrivateTag
    serialiseMLS msg
  serialiseMLS (MessagePublic msg) = do
    serialiseMLS WireFormatPublicTag
    serialiseMLS msg
  serialiseMLS (MessageWelcome welcome) = do
    serialiseMLS WireFormatWelcomeTag
    serialiseMLS welcome
  serialiseMLS (MessageGroupInfo gi) = do
    serialiseMLS WireFormatGroupInfoTag
    serialiseMLS gi
  serialiseMLS (MessageKeyPackage kp) = do
    serialiseMLS WireFormatKeyPackageTag
    serialiseMLS kp

instance S.ToSchema Message where
  declareNamedSchema _ = pure (mlsSwagger "MLSMessage")

data PublicMessage = PublicMessage
  { content :: RawMLS FramedContent,
    authData :: FramedContentAuthData,
    -- Present iff content.rmValue.sender is of type Member.
    membershipTag :: Maybe ByteString
  }
  deriving (Eq, Show)

instance ParseMLS PublicMessage where
  parseMLS = do
    content <- parseMLS
    authData <- parseFramedContentAuthData (framedContentDataTag (content.rmValue.content))
    membershipTag <- case content.rmValue.sender of
      SenderMember _ -> Just <$> parseMLSBytes @VarInt
      _ -> pure Nothing
    pure
      PublicMessage
        { content = content,
          authData = authData,
          membershipTag = membershipTag
        }

instance SerialiseMLS PublicMessage where
  serialiseMLS msg = do
    serialiseMLS msg.content
    serialiseMLS msg.authData
    traverse_ (serialiseMLSBytes @VarInt) msg.membershipTag

data PrivateMessage = PrivateMessage
  { groupId :: GroupId,
    epoch :: Epoch,
    tag :: FramedContentDataTag,
    authenticatedData :: ByteString,
    encryptedSenderData :: ByteString,
    ciphertext :: ByteString
  }
  deriving (Eq, Show)

instance ParseMLS PrivateMessage where
  parseMLS =
    PrivateMessage
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLSBytes @VarInt
      <*> parseMLSBytes @VarInt
      <*> parseMLSBytes @VarInt

data SenderTag
  = SenderMemberTag
  | SenderExternalTag
  | SenderNewMemberProposalTag
  | SenderNewMemberCommitTag
  deriving (Bounded, Enum, Show, Eq)

instance ParseMLS SenderTag where
  parseMLS = parseMLSEnum @Word8 "sender type"

instance SerialiseMLS SenderTag where
  serialiseMLS = serialiseMLSEnum @Word8

data Sender
  = SenderMember LeafIndex
  | SenderExternal Word32
  | SenderNewMemberProposal
  | SenderNewMemberCommit
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Sender)

instance ParseMLS Sender where
  parseMLS =
    parseMLS >>= \case
      SenderMemberTag -> SenderMember <$> parseMLS
      SenderExternalTag -> SenderExternal <$> parseMLS
      SenderNewMemberProposalTag -> pure SenderNewMemberProposal
      SenderNewMemberCommitTag -> pure SenderNewMemberCommit

instance SerialiseMLS Sender where
  serialiseMLS (SenderMember i) = do
    serialiseMLS SenderMemberTag
    serialiseMLS i
  serialiseMLS (SenderExternal w) = do
    serialiseMLS SenderExternalTag
    serialiseMLS w
  serialiseMLS SenderNewMemberProposal =
    serialiseMLS SenderNewMemberProposalTag
  serialiseMLS SenderNewMemberCommit =
    serialiseMLS SenderNewMemberCommitTag

needsGroupContext :: Sender -> Bool
needsGroupContext (SenderMember _) = True
needsGroupContext (SenderExternal _) = True
needsGroupContext _ = False

data FramedContent = FramedContent
  { groupId :: GroupId,
    epoch :: Epoch,
    sender :: Sender,
    authenticatedData :: ByteString,
    content :: FramedContentData
  }
  deriving (Eq, Show)

instance ParseMLS FramedContent where
  parseMLS =
    FramedContent
      <$> parseMLS
      <*> parseMLS
      <*> parseMLS
      <*> parseMLSBytes @VarInt
      <*> parseMLS

instance SerialiseMLS FramedContent where
  serialiseMLS fc = do
    serialiseMLS fc.groupId
    serialiseMLS fc.epoch
    serialiseMLS fc.sender
    serialiseMLSBytes @VarInt fc.authenticatedData
    serialiseMLS fc.content

data FramedContentDataTag
  = FramedContentApplicationDataTag
  | FramedContentProposalTag
  | FramedContentCommitTag
  deriving (Enum, Bounded, Eq, Ord, Show)

instance ParseMLS FramedContentDataTag where
  parseMLS = parseMLSEnum @Word8 "ContentType"

instance SerialiseMLS FramedContentDataTag where
  serialiseMLS = serialiseMLSEnum @Word8

data FramedContentData
  = FramedContentApplicationData ByteString
  | FramedContentProposal (RawMLS Proposal)
  | FramedContentCommit (RawMLS Commit)
  deriving (Eq, Show)

framedContentDataTag :: FramedContentData -> FramedContentDataTag
framedContentDataTag (FramedContentApplicationData _) = FramedContentApplicationDataTag
framedContentDataTag (FramedContentProposal _) = FramedContentProposalTag
framedContentDataTag (FramedContentCommit _) = FramedContentCommitTag

instance ParseMLS FramedContentData where
  parseMLS =
    parseMLS >>= \case
      FramedContentApplicationDataTag ->
        FramedContentApplicationData <$> parseMLSBytes @VarInt
      FramedContentProposalTag -> FramedContentProposal <$> parseMLS
      FramedContentCommitTag -> FramedContentCommit <$> parseMLS

instance SerialiseMLS FramedContentData where
  serialiseMLS (FramedContentApplicationData bs) = do
    serialiseMLS FramedContentApplicationDataTag
    serialiseMLSBytes @VarInt bs
  serialiseMLS (FramedContentProposal prop) = do
    serialiseMLS FramedContentProposalTag
    serialiseMLS prop
  serialiseMLS (FramedContentCommit commit) = do
    serialiseMLS FramedContentCommitTag
    serialiseMLS commit

data FramedContentTBS = FramedContentTBS
  { protocolVersion :: ProtocolVersion,
    wireFormat :: WireFormatTag,
    content :: RawMLS FramedContent,
    groupContext :: Maybe (RawMLS GroupContext)
  }
  deriving (Eq, Show)

instance SerialiseMLS FramedContentTBS where
  serialiseMLS tbs = do
    serialiseMLS tbs.protocolVersion
    serialiseMLS tbs.wireFormat
    serialiseMLS tbs.content
    traverse_ serialiseMLS tbs.groupContext

framedContentTBS :: RawMLS GroupContext -> RawMLS FramedContent -> FramedContentTBS
framedContentTBS ctx msgContent =
  FramedContentTBS
    { protocolVersion = defaultProtocolVersion,
      wireFormat = WireFormatPublicTag,
      content = msgContent,
      groupContext = guard (needsGroupContext msgContent.rmValue.sender) $> ctx
    }

data FramedContentAuthData = FramedContentAuthData
  { signature_ :: ByteString,
    -- Present iff it is part of a commit.
    confirmationTag :: Maybe ByteString
  }
  deriving (Eq, Show)

parseFramedContentAuthData :: FramedContentDataTag -> Get FramedContentAuthData
parseFramedContentAuthData tag = do
  sig <- parseMLSBytes @VarInt
  confirmationTag <- case tag of
    FramedContentCommitTag -> Just <$> parseMLSBytes @VarInt
    _ -> pure Nothing
  pure (FramedContentAuthData sig confirmationTag)

instance SerialiseMLS FramedContentAuthData where
  serialiseMLS ad = do
    serialiseMLSBytes @VarInt ad.signature_
    traverse_ (serialiseMLSBytes @VarInt) ad.confirmationTag

data GroupContext = GroupContext
  { protocolVersion :: ProtocolVersion,
    cipherSuite :: CipherSuite,
    groupId :: GroupId,
    epoch :: Epoch,
    treeHash :: ByteString,
    confirmedTranscriptHash :: ByteString,
    extensions :: [Extension]
  }
  deriving (Eq, Show)

-- | Craft a message with the backend itself as a sender.
mkSignedMessage ::
  SecretKey -> PublicKey -> GroupId -> Epoch -> FramedContentData -> Message
mkSignedMessage priv pub gid epoch payload =
  let framedContent =
        mkRawMLS
          FramedContent
            { groupId = gid,
              epoch = epoch,
              sender = SenderExternal 0,
              content = payload,
              authenticatedData = mempty
            }
      tbs =
        FramedContentTBS
          { protocolVersion = defaultProtocolVersion,
            wireFormat = WireFormatPublicTag,
            content = framedContent,
            groupContext = Nothing
          }
      sig = BA.convert $ sign priv pub (encodeMLS' tbs)
   in mkMessage $
        MessagePublic
          PublicMessage
            { content = framedContent,
              authData = FramedContentAuthData sig Nothing,
              membershipTag = Nothing
            }

verifyMessageSignature ::
  RawMLS GroupContext ->
  RawMLS FramedContent ->
  FramedContentAuthData ->
  ByteString ->
  Bool
verifyMessageSignature ctx msgContent authData pubkey = isJust $ do
  let tbs = mkRawMLS (framedContentTBS ctx msgContent)
      sig = authData.signature_
  cs <- cipherSuiteTag ctx.rmValue.cipherSuite
  guard $ csVerifySignature cs pubkey tbs sig

--------------------------------------------------------------------------------
-- Servant

newtype UnreachableUsers = UnreachableUsers {unreachableUsers :: [Qualified UserId]}
  deriving stock (Eq, Show)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema UnreachableUsers
  deriving newtype (Semigroup, Monoid)

instance ToSchema UnreachableUsers where
  schema =
    named "UnreachableUsers" $
      UnreachableUsers
        <$> unreachableUsers
          .= array schema

data MLSMessageSendingStatus = MLSMessageSendingStatus
  { mmssEvents :: [Event],
    mmssTime :: UTCTimeMillis,
    mmssUnreachableUsers :: UnreachableUsers
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
        <*> mmssUnreachableUsers
          .= fieldWithDocModifier
            "failed_to_send"
            (description ?~ "List of federated users who could not be reached and did not receive the message")
            schema
