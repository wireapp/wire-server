{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
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

module Wire.API.Event.Conversation
  ( -- * Event
    Event (..),
    evtType,
    EventType (..),
    EventData (..),
    AddCodeResult (..),

    -- * Event lenses
    _EdMembersJoin,
    _EdMembersLeave,
    _EdConnect,
    _EdConvReceiptModeUpdate,
    _EdConvRename,
    _EdConvDelete,
    _EdConvAccessUpdate,
    _EdConvMessageTimerUpdate,
    _EdConvCodeUpdate,
    _EdConvCodeDelete,
    _EdMemberUpdate,
    _EdConversation,
    _EdTyping,
    _EdOtrMessage,
    _EdMLSMessage,
    _EdMLSWelcome,

    -- * Event data helpers
    SimpleMember (..),
    smId,
    SimpleMembers (..),
    Connect (..),
    MemberUpdateData (..),
    OtrMessage (..),

    -- * re-exports
    ConversationReceiptModeUpdate (..),
    ConversationRename (..),
    ConversationAccessData (..),
    ConversationMessageTimerUpdate (..),
    ConversationCode (..),
    QualifiedUserIdList (..),
  )
where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens (makePrisms, (?~), _1)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Id
import Data.Json.Util
import Data.OpenApi (deprecated)
import Data.OpenApi qualified as S
import Data.Qualified
import Data.SOP
import Data.Schema
import Data.Time
import Imports
import Test.QuickCheck qualified as QC
import URI.ByteString ()
import Wire.API.Conversation
import Wire.API.Conversation.Code (ConversationCode (..), ConversationCodeInfo)
import Wire.API.Conversation.Protocol (ProtocolUpdate (unProtocolUpdate))
import Wire.API.Conversation.Protocol qualified as P
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Event.LeaveReason
import Wire.API.MLS.SubConversation
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Version
import Wire.API.User (QualifiedUserIdList (..), qualifiedUserIdListObjectSchema)
import Wire.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

--------------------------------------------------------------------------------
-- Event

data Event = Event
  { evtConv :: Qualified ConvId,
    evtSubConv :: Maybe SubConvId,
    evtFrom :: Qualified UserId,
    evtTime :: UTCTime,
    evtData :: EventData
  }
  deriving stock (Eq, Show, Generic)

evtType :: Event -> EventType
evtType = eventDataType . evtData

instance Arbitrary Event where
  arbitrary = do
    typ <- arbitrary
    Event
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> (milli <$> arbitrary)
      <*> genEventData typ
    where
      milli = fromUTCTimeMillis . toUTCTimeMillis

data EventType
  = MemberJoin
  | MemberLeave
  | MemberStateUpdate
  | ConvRename
  | ConvAccessUpdate
  | ConvMessageTimerUpdate
  | ConvCodeUpdate
  | ConvCodeDelete
  | ConvCreate
  | ConvConnect
  | ConvDelete
  | ConvReceiptModeUpdate
  | OtrMessageAdd
  | MLSMessageAdd
  | MLSWelcome
  | Typing
  | ProtocolUpdate
  | ConvGroupPictureUpdate
  deriving stock (Eq, Show, Generic, Enum, Bounded, Ord)
  deriving (Arbitrary) via (GenericUniform EventType)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema EventType

instance ToSchema EventType where
  schema =
    enum @Text "EventType" $
      mconcat
        [ element "conversation.member-join" MemberJoin,
          element "conversation.member-leave" MemberLeave,
          element "conversation.member-update" MemberStateUpdate,
          element "conversation.rename" ConvRename,
          element "conversation.access-update" ConvAccessUpdate,
          element "conversation.receipt-mode-update" ConvReceiptModeUpdate,
          element "conversation.message-timer-update" ConvMessageTimerUpdate,
          element "conversation.code-update" ConvCodeUpdate,
          element "conversation.code-delete" ConvCodeDelete,
          element "conversation.create" ConvCreate,
          element "conversation.delete" ConvDelete,
          element "conversation.connect-request" ConvConnect,
          element "conversation.typing" Typing,
          element "conversation.otr-message-add" OtrMessageAdd,
          element "conversation.mls-message-add" MLSMessageAdd,
          element "conversation.mls-welcome" MLSWelcome,
          element "conversation.protocol-update" ProtocolUpdate
        ]

data EventData
  = EdMembersJoin SimpleMembers
  | EdMembersLeave EdMemberLeftReason QualifiedUserIdList
  | EdConnect Connect
  | EdConvReceiptModeUpdate ConversationReceiptModeUpdate
  | EdConvRename ConversationRename
  | EdConvDelete
  | EdConvAccessUpdate ConversationAccessData
  | EdConvMessageTimerUpdate ConversationMessageTimerUpdate
  | EdConvCodeUpdate ConversationCodeInfo
  | EdConvCodeDelete
  | EdMemberUpdate MemberUpdateData
  | EdConversation Conversation
  | EdTyping TypingStatus
  | EdOtrMessage OtrMessage
  | EdMLSMessage ByteString
  | EdMLSWelcome ByteString
  | EdProtocolUpdate P.ProtocolTag
  | EdConvGroupPictureUpdate ConversationGroupPicture
  deriving stock (Eq, Show, Generic)

genEventData :: EventType -> QC.Gen EventData
genEventData = \case
  MemberJoin -> EdMembersJoin <$> arbitrary
  MemberLeave -> EdMembersLeave <$> arbitrary <*> arbitrary
  MemberStateUpdate -> EdMemberUpdate <$> arbitrary
  ConvRename -> EdConvRename <$> arbitrary
  ConvAccessUpdate -> EdConvAccessUpdate <$> arbitrary
  ConvMessageTimerUpdate -> EdConvMessageTimerUpdate <$> arbitrary
  ConvCodeUpdate -> EdConvCodeUpdate <$> arbitrary
  ConvCodeDelete -> pure EdConvCodeDelete
  ConvConnect -> EdConnect <$> arbitrary
  ConvCreate -> EdConversation <$> arbitrary
  ConvReceiptModeUpdate -> EdConvReceiptModeUpdate <$> arbitrary
  Typing -> EdTyping <$> arbitrary
  OtrMessageAdd -> EdOtrMessage <$> arbitrary
  MLSMessageAdd -> EdMLSMessage <$> arbitrary
  MLSWelcome -> EdMLSWelcome <$> arbitrary
  ConvDelete -> pure EdConvDelete
  ProtocolUpdate -> EdProtocolUpdate <$> arbitrary
  ConvGroupPictureUpdate -> EdConvGroupPictureUpdate <$> arbitrary

eventDataType :: EventData -> EventType
eventDataType (EdMembersJoin _) = MemberJoin
eventDataType (EdMembersLeave _ _) = MemberLeave
eventDataType (EdMemberUpdate _) = MemberStateUpdate
eventDataType (EdConvRename _) = ConvRename
eventDataType (EdConvAccessUpdate _) = ConvAccessUpdate
eventDataType (EdConvMessageTimerUpdate _) = ConvMessageTimerUpdate
eventDataType (EdConvCodeUpdate _) = ConvCodeUpdate
eventDataType EdConvCodeDelete = ConvCodeDelete
eventDataType (EdConnect _) = ConvConnect
eventDataType (EdConversation _) = ConvCreate
eventDataType (EdConvReceiptModeUpdate _) = ConvReceiptModeUpdate
eventDataType (EdTyping _) = Typing
eventDataType (EdOtrMessage _) = OtrMessageAdd
eventDataType (EdMLSMessage _) = MLSMessageAdd
eventDataType (EdMLSWelcome _) = MLSWelcome
eventDataType EdConvDelete = ConvDelete
eventDataType (EdProtocolUpdate _) = ProtocolUpdate
eventDataType (EdConvGroupPictureUpdate _) = ConvGroupPictureUpdate

--------------------------------------------------------------------------------
-- Event data helpers

newtype SimpleMembers = SimpleMembers
  { mMembers :: [SimpleMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema SimpleMembers

instance ToSchema SimpleMembers where
  schema =
    object "SimpleMembers" $
      SimpleMembers
        <$> mMembers .= field "users" (array schema)
        <* (fmap smId . mMembers)
          .= optional
            ( fieldWithDocModifier
                "user_ids"
                ( (description ?~ "deprecated")
                    . (deprecated ?~ True)
                )
                (array schema)
            )

data SimpleMember = SimpleMember
  { smQualifiedId :: Qualified UserId,
    smConvRoleName :: RoleName
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SimpleMember)
  deriving (FromJSON, ToJSON) via Schema SimpleMember

smId :: SimpleMember -> UserId
smId = qUnqualified . smQualifiedId

instance ToSchema SimpleMember where
  schema =
    object "SimpleMember" $
      SimpleMember
        <$> smQualifiedId .= field "qualified_id" schema
        <* smId .= optional (field "id" schema)
        <*> smConvRoleName
          .= (field "conversation_role" schema <|> pure roleNameWireAdmin)

data Connect = Connect
  { cRecipient :: Qualified UserId,
    -- FUTUREWORK: As a follow-up from
    -- https://github.com/wireapp/wire-server/pull/1726, the message field can
    -- be removed from this event.
    cMessage :: Maybe Text,
    cName :: Maybe Text,
    cEmail :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Connect)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Connect

instance ToSchema Connect where
  schema = object "Connect" connectObjectSchema

connectObjectSchema :: ObjectSchema SwaggerDoc Connect
connectObjectSchema =
  Connect
    <$> cRecipient .= field "qualified_recipient" schema
    <* (qUnqualified . cRecipient) .= optional (field "recipient" schema)
    <*> cMessage .= optField "message" (maybeWithDefault A.Null schema)
    <*> cName .= optField "name" (maybeWithDefault A.Null schema)
    <*> cEmail .= optField "email" (maybeWithDefault A.Null schema)

-- | Outbound member updates. When a user A acts upon a user B,
-- then a user event is generated where B's user ID is set
-- as misTarget.
-- Used for events (sent over the websocket, etc.).  See also
-- 'MemberUpdate' and 'OtherMemberUpdate'.
data MemberUpdateData = MemberUpdateData
  { -- | Target user of this action
    misTarget :: Qualified UserId,
    misOtrMutedStatus :: Maybe MutedStatus,
    misOtrMutedRef :: Maybe Text,
    misOtrArchived :: Maybe Bool,
    misOtrArchivedRef :: Maybe Text,
    misHidden :: Maybe Bool,
    misHiddenRef :: Maybe Text,
    misConvRoleName :: Maybe RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform MemberUpdateData)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema MemberUpdateData

instance ToSchema MemberUpdateData where
  schema = object "MemberUpdateData" memberUpdateDataObjectSchema

memberUpdateDataObjectSchema :: ObjectSchema SwaggerDoc MemberUpdateData
memberUpdateDataObjectSchema =
  MemberUpdateData
    <$> misTarget .= field "qualified_target" schema
    <* (qUnqualified . misTarget) .= optional (field "target" schema)
    <*> misOtrMutedStatus .= maybe_ (optField "otr_muted_status" schema)
    <*> misOtrMutedRef .= maybe_ (optField "otr_muted_ref" schema)
    <*> misOtrArchived .= maybe_ (optField "otr_archived" schema)
    <*> misOtrArchivedRef .= maybe_ (optField "otr_archived_ref" schema)
    <*> misHidden .= maybe_ (optField "hidden" schema)
    <*> misHiddenRef .= maybe_ (optField "hidden_ref" schema)
    <*> misConvRoleName .= maybe_ (optField "conversation_role" schema)

data AddCodeResult
  = CodeAdded Event
  | CodeAlreadyExisted ConversationCodeInfo

data OtrMessage = OtrMessage
  { otrSender :: ClientId,
    otrRecipient :: ClientId,
    otrCiphertext :: Text,
    otrData :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OtrMessage)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema OtrMessage

instance ToSchema OtrMessage where
  schema =
    objectWithDocModifier
      "OtrMessage"
      (description ?~ "Encrypted message of a conversation")
      otrMessageObjectSchema

otrMessageObjectSchema :: ObjectSchema SwaggerDoc OtrMessage
otrMessageObjectSchema =
  OtrMessage
    <$> otrSender .= field "sender" schema
    <*> otrRecipient .= field "recipient" schema
    <*> otrCiphertext
      .= fieldWithDocModifier
        "text"
        (description ?~ textDesc)
        schema
    <*> otrData
      .= maybe_
        ( optFieldWithDocModifier
            "data"
            (description ?~ dataDesc)
            schema
        )
  where
    textDesc = "The ciphertext for the recipient (Base64 in JSON)"
    dataDesc =
      "Extra (symmetric) data (i.e. ciphertext, Base64 in JSON) \
      \that is common with all other recipients."

makePrisms ''EventData

taggedEventDataSchema :: ObjectSchema SwaggerDoc (EventType, EventData)
taggedEventDataSchema =
  bind
    (fst .= field "type" schema)
    (snd .= fieldOver _1 "data" edata)
  where
    edata = dispatch $ \case
      MemberJoin -> tag _EdMembersJoin (unnamed schema)
      MemberLeave -> tag _EdMembersLeave (unnamed memberLeaveSchema)
      MemberStateUpdate -> tag _EdMemberUpdate (unnamed schema)
      ConvRename -> tag _EdConvRename (unnamed schema)
      -- FUTUREWORK: when V2 is dropped, it is fine to change this schema to
      -- V3, since V3 clients are guaranteed to know how to parse V2 and V3
      -- conversation access update events.
      ConvAccessUpdate ->
        tag
          _EdConvAccessUpdate
          (unnamed (conversationAccessDataSchema (Just V2)))
      ConvCodeUpdate -> tag _EdConvCodeUpdate (unnamed schema)
      ConvConnect -> tag _EdConnect (unnamed schema)
      ConvCreate -> tag _EdConversation (unnamed (conversationSchema (Just V2)))
      ConvMessageTimerUpdate -> tag _EdConvMessageTimerUpdate (unnamed schema)
      ConvReceiptModeUpdate -> tag _EdConvReceiptModeUpdate (unnamed schema)
      OtrMessageAdd -> tag _EdOtrMessage (unnamed schema)
      MLSMessageAdd -> tag _EdMLSMessage base64Schema
      MLSWelcome -> tag _EdMLSWelcome base64Schema
      Typing -> tag _EdTyping (unnamed schema)
      ConvCodeDelete -> tag _EdConvCodeDelete null_
      ConvDelete -> tag _EdConvDelete null_
      ProtocolUpdate -> tag _EdProtocolUpdate (unnamed (unProtocolUpdate <$> P.ProtocolUpdate .= schema))
      ConvGroupPictureUpdate -> tag _EdConvGroupPictureUpdate (unnamed schema)

memberLeaveSchema :: ValueSchema NamedSwaggerDoc (EdMemberLeftReason, QualifiedUserIdList)
memberLeaveSchema =
  object "QualifiedUserIdList with EdMemberLeftReason" $
    (,) <$> fst .= field "reason" schema <*> snd .= qualifiedUserIdListObjectSchema

instance ToSchema Event where
  schema = object "Event" eventObjectSchema

eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema =
  mk
    <$> (evtType &&& evtData) .= taggedEventDataSchema
    <* (qUnqualified . evtConv) .= optional (field "conversation" schema)
    <*> evtConv .= field "qualified_conversation" schema
    <*> evtSubConv .= maybe_ (optField "subconv" schema)
    <* (qUnqualified . evtFrom) .= optional (field "from" schema)
    <*> evtFrom .= field "qualified_from" schema
    <*> (toUTCTimeMillis . evtTime) .= field "time" (fromUTCTimeMillis <$> schema)
  where
    mk (_, d) cid sconvid uid tm = Event cid sconvid uid tm d

instance ToJSONObject Event where
  toJSONObject =
    KeyMap.fromList
      . fromMaybe []
      . schemaOut eventObjectSchema

instance FromJSON Event where
  parseJSON = schemaParseJSON

instance ToJSON Event where
  toJSON = schemaToJSON

instance S.ToSchema Event where
  declareNamedSchema = schemaToSwagger

--------------------------------------------------------------------------------
-- MultiVerb instances

instance
  (ResponseType r1 ~ ConversationCodeInfo, ResponseType r2 ~ Event) =>
  AsUnion '[r1, r2] AddCodeResult
  where
  toUnion (CodeAlreadyExisted c) = Z (I c)
  toUnion (CodeAdded e) = S (Z (I e))

  fromUnion (Z (I c)) = CodeAlreadyExisted c
  fromUnion (S (Z (I e))) = CodeAdded e
  fromUnion (S (S x)) = case x of {}
