{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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
  )
where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens (makePrisms, (?~), _1)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Id
import Data.Json.Util (ToJSONObject (toJSONObject), UTCTimeMillis (fromUTCTimeMillis), toUTCTimeMillis)
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import Data.Time
import Imports
import qualified Test.QuickCheck as QC
import URI.ByteString ()
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))
import Wire.API.Conversation
import Wire.API.Conversation.Code (ConversationCode (..))
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing (TypingData (..))
import Wire.API.User (QualifiedUserIdList (..))

--------------------------------------------------------------------------------
-- Event

data Event = Event
  { evtConv :: Qualified ConvId,
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
  | EdMLSMessage ByteString
  deriving stock (Eq, Show, Generic)

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
  MLSMessageAdd -> EdMLSMessage <$> arbitrary
  ConvDelete -> pure EdConvDelete

eventDataType :: EventData -> EventType
eventDataType (EdMembersJoin _) = MemberJoin
eventDataType (EdMembersLeave _) = MemberLeave
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
eventDataType EdConvDelete = ConvDelete

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
  | CodeAlreadyExisted ConversationCode

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
      MLSMessageAdd -> tag _EdOtrMessage (unnamed schema)
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
    mk (_, d) cid uid tm = Event cid uid tm d

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
