{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
    EventType (..),
    EventData (..),

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
    Conversation (..),
    TypingData (..),
    QualifiedUserIdList (..),

    -- * Swagger
    modelEvent,
    modelMemberEvent,
    modelConnectEvent,
    modelConversationReceiptModeUpdateEvent,
    modelConversationNameUpdateEvent,
    modelConversationAccessUpdateEvent,
    modelConversationMessageTimerUpdateEvent,
    modelConversationCodeUpdateEvent,
    modelConversationCodeDeleteEvent,
    modelMemberUpdateEvent,
    modelTypingEvent,
    modelOtrMessageEvent,
    modelMembers,
    modelConnect,
    modelMemberUpdateData,
    modelOtrMessage,
    typeEventType,
  )
where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens (makePrisms, (?~), _1)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util (ToJSONObject (toJSONObject), UTCTimeMillis (fromUTCTimeMillis), toUTCTimeMillis)
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Data.Time
import Imports
import qualified Test.QuickCheck as QC
import URI.ByteString ()
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))
import Wire.API.Conversation
import Wire.API.Conversation.Code (ConversationCode (..), modelConversationCode)
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing (TypingData (..), modelTyping)
import Wire.API.User (QualifiedUserIdList (..))

--------------------------------------------------------------------------------
-- Event

data Event = Event
  { evtType :: EventType,
    evtConv :: Qualified ConvId,
    evtFrom :: Qualified UserId,
    evtTime :: UTCTime,
    evtData :: EventData
  }
  deriving stock (Eq, Show, Generic)

modelEvent :: Doc.Model
modelEvent = Doc.defineModel "Event" $ do
  Doc.description "Event data"
  Doc.property "type" typeEventType $
    Doc.description "Event type"
  Doc.property "conversation" Doc.bytes' $
    Doc.description "Conversation ID"
  Doc.property "from" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "time" Doc.dateTime' $
    Doc.description "Date and time this event occurred"
  -- This doesn't really seem to work in swagger-ui.
  -- The children/subTypes are not displayed.
  Doc.children
    "type"
    [ modelMemberEvent,
      modelConnectEvent,
      modelConversationReceiptModeUpdateEvent,
      modelConversationNameUpdateEvent,
      modelConversationAccessUpdateEvent,
      modelConversationMessageTimerUpdateEvent,
      modelConversationCodeUpdateEvent,
      modelConversationCodeDeleteEvent,
      modelMemberUpdateEvent,
      modelTypingEvent,
      modelOtrMessageEvent
    ]

instance Arbitrary Event where
  arbitrary = do
    typ <- arbitrary
    Event typ
      <$> arbitrary
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
  | Typing
  deriving stock (Eq, Show, Generic, Enum, Bounded)
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
          element "conversation.otr-message-add" OtrMessageAdd
        ]

typeEventType :: Doc.DataType
typeEventType =
  Doc.string $
    Doc.enum
      [ "conversation.member-join",
        "conversation.member-leave",
        "conversation.member-update",
        "conversation.rename",
        "conversation.access-update",
        "conversation.receipt-mode-update",
        "conversation.message-timer-update",
        "conversation.code-update",
        "conversation.code-delete",
        "conversation.create",
        "conversation.delete",
        "conversation.connect-request",
        "conversation.typing",
        "conversation.otr-message-add"
      ]

data EventData
  = EdMembersJoin SimpleMembers
  | EdMembersLeave QualifiedUserIdList
  | EdConnect Connect
  | EdConvReceiptModeUpdate ConversationReceiptModeUpdate
  | EdConvRename ConversationRename
  | EdConvDelete
  | EdConvAccessUpdate ConversationAccessData
  | EdConvMessageTimerUpdate ConversationMessageTimerUpdate
  | EdConvCodeUpdate ConversationCode
  | EdConvCodeDelete
  | EdMemberUpdate MemberUpdateData
  | EdConversation Conversation
  | EdTyping TypingData
  | EdOtrMessage OtrMessage
  deriving stock (Eq, Show, Generic)

modelMemberEvent :: Doc.Model
modelMemberEvent = Doc.defineModel "MemberEvent" $ do
  Doc.description "member event"
  Doc.property "data" (Doc.ref modelMembers) $ Doc.description "members data"

modelConnectEvent :: Doc.Model
modelConnectEvent = Doc.defineModel "ConnectEvent" $ do
  Doc.description "connect event"
  Doc.property "data" (Doc.ref modelConnect) $ Doc.description "connect data"

modelConversationReceiptModeUpdateEvent :: Doc.Model
modelConversationReceiptModeUpdateEvent = Doc.defineModel "ConversationReceiptModeUpdateEvent" $ do
  Doc.description "conversation receipt mode update event"
  Doc.property "data" (Doc.ref modelConversationReceiptModeUpdate) $ Doc.description "conversation receipt mode data"

modelConversationNameUpdateEvent :: Doc.Model
modelConversationNameUpdateEvent = Doc.defineModel "ConversationNameUpdateEvent" $ do
  Doc.description "conversation update event"
  Doc.property "data" (Doc.ref modelConversationUpdateName) $ Doc.description "conversation name"

modelConversationAccessUpdateEvent :: Doc.Model
modelConversationAccessUpdateEvent = Doc.defineModel "ConversationAccessUpdateEvent" $ do
  Doc.description "conversation access update event"
  Doc.property "data" (Doc.ref modelConversationAccessData) $ Doc.description "conversation access data"

modelConversationMessageTimerUpdateEvent :: Doc.Model
modelConversationMessageTimerUpdateEvent = Doc.defineModel "ConversationMessageTimerUpdateEvent" $ do
  Doc.description "conversation message timer update event"
  Doc.property "data" (Doc.ref modelConversationMessageTimerUpdate) $ Doc.description "conversation message timer data"

modelConversationCodeUpdateEvent :: Doc.Model
modelConversationCodeUpdateEvent = Doc.defineModel "ConversationCodeUpdateEvent" $ do
  Doc.description "conversation code update event"
  Doc.property "data" (Doc.ref modelConversationCode) $ Doc.description "conversation code data"

modelConversationCodeDeleteEvent :: Doc.Model
modelConversationCodeDeleteEvent =
  Doc.defineModel "ConversationCodeDeleteEvent" $
    Doc.description "conversation code delete event"

modelMemberUpdateEvent :: Doc.Model
modelMemberUpdateEvent = Doc.defineModel "MemberUpdateEvent" $ do
  Doc.description "member update event"
  Doc.property "data" (Doc.ref modelMemberUpdateData) $ Doc.description "member data"

modelTypingEvent :: Doc.Model
modelTypingEvent = Doc.defineModel "TypingEvent" $ do
  Doc.description "typing event"
  Doc.property "data" (Doc.ref modelTyping) $ Doc.description "typing data"

modelOtrMessageEvent :: Doc.Model
modelOtrMessageEvent = Doc.defineModel "OtrMessage" $ do
  Doc.description "off-the-record message event"
  Doc.property "data" (Doc.ref modelOtrMessage) $ Doc.description "OTR message"

genEventData :: EventType -> QC.Gen EventData
genEventData = \case
  MemberJoin -> EdMembersJoin <$> arbitrary
  MemberLeave -> EdMembersLeave <$> arbitrary
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
  ConvDelete -> pure EdConvDelete

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
                (description ?~ "deprecated")
                (array schema)
            )

-- | Used both for 'SimpleMembers' and 'UserIdList'.
modelMembers :: Doc.Model
modelMembers =
  Doc.defineModel "Members" $
    Doc.property "users" (Doc.unique $ Doc.array Doc.bytes') $
      Doc.description "List of user IDs"

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

modelConnect :: Doc.Model
modelConnect = Doc.defineModel "Connect" $ do
  Doc.description "user to user connection request"
  Doc.property "recipient" Doc.bytes' $
    Doc.description "The user ID to connect to"
  Doc.property "message" Doc.string' $
    Doc.description "Initial message to send to user"
  Doc.property "name" Doc.string' $
    Doc.description "Name of requestor"
  Doc.property "email" Doc.string' $ do
    Doc.description "E-Mail of requestor"
    Doc.optional

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

modelMemberUpdateData :: Doc.Model
modelMemberUpdateData = Doc.defineModel "MemberUpdateData" $ do
  Doc.description "Event data on member updates"
  Doc.property "target" Doc.bytes' $ do
    Doc.description "Target ID of the user that the action was performed on"
    Doc.optional
  Doc.property "otr_muted_ref" Doc.bytes' $ do
    Doc.description "A reference point for (un)muting"
    Doc.optional
  Doc.property "otr_archived" Doc.bool' $ do
    Doc.description "Whether to notify on conversation updates"
    Doc.optional
  Doc.property "otr_archived_ref" Doc.bytes' $ do
    Doc.description "A reference point for (un)archiving"
    Doc.optional
  Doc.property "hidden" Doc.bool' $ do
    Doc.description "Whether the conversation is hidden"
    Doc.optional
  Doc.property "hidden_ref" Doc.bytes' $ do
    Doc.description "A reference point for (un)hiding"
    Doc.optional
  Doc.property "conversation_role" Doc.string' $ do
    Doc.description "Name of the conversation role to update to"
    Doc.optional

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

modelOtrMessage :: Doc.Model
modelOtrMessage = Doc.defineModel "OtrMessage" $ do
  Doc.description "Encrypted message of a conversation"
  Doc.property "sender" Doc.bytes' $
    Doc.description "The sender's client ID"
  Doc.property "recipient" Doc.bytes' $
    Doc.description "The recipient's client ID"
  Doc.property "text" Doc.bytes' $
    Doc.description "The ciphertext for the recipient (Base64 in JSON)"
  Doc.property "data" Doc.bytes' $ do
    Doc.description
      "Extra (symmetric) data (i.e. ciphertext, Base64 in JSON) \
      \that is common with all other recipients."
    Doc.optional

makePrisms ''EventData

taggedEventDataSchema :: ObjectSchema SwaggerDoc (EventType, EventData)
taggedEventDataSchema =
  bind
    (fst .= field "type" schema)
    (snd .= fieldOver _1 "data" edata)
  where
    edata = dispatch $ \case
      MemberJoin -> tag _EdMembersJoin (unnamed schema)
      MemberLeave -> tag _EdMembersLeave (unnamed schema)
      MemberStateUpdate -> tag _EdMemberUpdate (unnamed schema)
      ConvRename -> tag _EdConvRename (unnamed schema)
      ConvAccessUpdate -> tag _EdConvAccessUpdate (unnamed schema)
      ConvCodeUpdate -> tag _EdConvCodeUpdate (unnamed schema)
      ConvConnect -> tag _EdConnect (unnamed schema)
      ConvCreate -> tag _EdConversation (unnamed schema)
      ConvMessageTimerUpdate -> tag _EdConvMessageTimerUpdate (unnamed schema)
      ConvReceiptModeUpdate -> tag _EdConvReceiptModeUpdate (unnamed schema)
      OtrMessageAdd -> tag _EdOtrMessage (unnamed schema)
      Typing -> tag _EdTyping (unnamed schema)
      ConvCodeDelete -> tag _EdConvCodeDelete null_
      ConvDelete -> tag _EdConvDelete null_

instance ToSchema Event where
  schema = object "Event" eventObjectSchema

eventObjectSchema :: ObjectSchema SwaggerDoc Event
eventObjectSchema =
  mk
    <$> (evtType &&& evtData) .= taggedEventDataSchema
    <* (qUnqualified . evtConv) .= optional (field "conversation" schema)
    <*> evtConv .= field "qualified_conversation" schema
    <* (qUnqualified . evtFrom) .= optional (field "from" schema)
    <*> evtFrom .= field "qualified_from" schema
    <*> (toUTCTimeMillis . evtTime) .= field "time" (fromUTCTimeMillis <$> schema)
  where
    mk (ty, d) cid uid tm = Event ty cid uid tm d

instance ToJSONObject Event where
  toJSONObject =
    HashMap.fromList
      . fromMaybe []
      . schemaOut eventObjectSchema

instance FromJSON Event where
  parseJSON = schemaParseJSON

instance ToJSON Event where
  toJSON = schemaToJSON

instance S.ToSchema Event where
  declareNamedSchema = schemaToSwagger
