{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

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

module Galley.Types
  ( -- * Galley conversation types
    Conversation (..),
    Member (..),
    ConvMembers (..),
    OtherMember (..),
    Connect (..),
    NewOtrMessage (..),
    ClientMismatch (..),
    OtrRecipients (..),
    foldrOtrRecipients,
    OtrFilterMissing (..),
    ConvTeamInfo (..),
    ConversationCode (..),
    mkConversationCode,

    -- * Events
    Event (..),
    EventType (..),
    EventData (..),
    OpaqueUserIdList (..),
    SimpleMember (..),
    SimpleMembers (..),
    MemberUpdateData (..),
    TypingData (..),
    OtrMessage (..),
    parseEventData,

    -- * Other galley types
    Access (..),
    AccessRole (..),
    Accept (..),
    ConversationList (..),
    ConversationMeta (..),
    ConversationRename (..),
    ConversationAccessUpdate (..),
    ConversationReceiptModeUpdate (..),
    ConversationMessageTimerUpdate (..),
    ConvType (..),
    CustomBackend (..),
    Invite (..),
    NewConv (..),
    NewConvManaged (..),
    NewConvUnmanaged (..),
    MemberUpdate (..),
    OtherMemberUpdate (..),
    MutedStatus (..),
    ReceiptMode (..),
    TypingStatus (..),
    UserClientMap (..),
    UserClients (..),
    UserIdList (..),
    filterClients,
    newInvite,
    memberUpdate,
  )
where

import Control.Lens ((.~))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Conversion
import qualified Data.Code as Code
import qualified Data.HashMap.Strict as HashMap
import Data.Id (ClientId (ClientId), ConvId, Id (Id, toUUID), OpaqueConvId, OpaqueUserId, TeamId, UserId)
import Data.Json.Util
import Data.List1
import qualified Data.Map.Strict as Map
import Data.Misc
import qualified Data.Text.Encoding as T
import Data.Time
import Data.UUID (toASCIIBytes)
import Galley.Types.Bot.Service (ServiceRef)
import Galley.Types.Conversations.Roles
import Gundeck.Types.Push (Priority)
import Imports
import URI.ByteString

-- Conversations ------------------------------------------------------------

-- | Public-facing conversation type. Represents information that a
-- particular user is allowed to see.
--
-- Can be produced from the internal one ('Galley.Data.Types.Conversation')
-- by using 'Galley.API.Mapping.conversationView'.
data Conversation
  = Conversation
      { cnvId :: !ConvId, -- FUTUREWORK(federation): make opaque
        cnvType :: !ConvType,
        cnvCreator :: !UserId,
        cnvAccess :: ![Access],
        cnvAccessRole :: !AccessRole,
        cnvName :: !(Maybe Text),
        cnvMembers :: !ConvMembers,
        cnvTeam :: !(Maybe TeamId),
        cnvMessageTimer :: !(Maybe Milliseconds),
        cnvReceiptMode :: !(Maybe ReceiptMode)
      }
  deriving (Eq, Show)

data ConvType
  = RegularConv
  | SelfConv
  | One2OneConv
  | ConnectConv
  deriving (Eq, Show)

-- | Define whether receipts should be sent in the given conversation
--   This datatype is defined as an int32 but the Backend does not
--   interpret it in any way, rather just stores and forwards it
--   for clients
--   E.g. of an implementation: 0 - send no ReceiptModes
--                              1 - send read ReceiptModes
--                              2 - send delivery ReceiptModes
--                              ...
newtype ReceiptMode = ReceiptMode {unReceiptMode :: Int32} deriving (Eq, Ord, Show)

-- | Access define how users can join conversations
data Access
  = -- | Made obsolete by PrivateAccessRole
    PrivateAccess
  | -- | User A can add User B
    InviteAccess
  | -- | User can join knowing conversation id
    LinkAccess
  | -- | User can join knowing [changeable/revokable] code
    CodeAccess
  deriving (Eq, Ord, Bounded, Enum, Show)

-- | AccessRoles define who can join conversations. The roles are
-- "supersets", i.e. Activated includes Team and NonActivated includes
-- Activated.
data AccessRole
  = -- | Nobody can be invited to this conversation
    --   (e.g. it's a 1:1 conversation)
    PrivateAccessRole
  | -- | Team-only conversation
    TeamAccessRole
  | -- | Conversation for users who have activated
    --   email or phone
    ActivatedAccessRole
  | -- | No checks
    NonActivatedAccessRole
  deriving (Eq, Ord, Show)

data ConvMembers
  = ConvMembers
      { cmSelf :: !Member,
        cmOthers :: ![OtherMember]
      }
  deriving (Eq, Show)

data ConversationMeta
  = ConversationMeta
      { cmId :: !ConvId,
        cmType :: !ConvType,
        cmCreator :: !UserId,
        cmAccess :: ![Access],
        cmAccessRole :: !AccessRole,
        cmName :: !(Maybe Text),
        cmTeam :: !(Maybe TeamId),
        cmMessageTimer :: !(Maybe Milliseconds),
        cmReceiptMode :: !(Maybe ReceiptMode)
      }
  deriving (Eq, Show)

data ConversationList a
  = ConversationList
      { convList :: [a],
        convHasMore :: !Bool
      }
  deriving (Eq, Show)

newtype ConversationRename
  = ConversationRename
      { cupName :: Text
      }

deriving instance Eq ConversationRename

deriving instance Show ConversationRename

data ConversationAccessUpdate
  = ConversationAccessUpdate
      { cupAccess :: [Access],
        cupAccessRole :: AccessRole
      }
  deriving (Eq, Show)

data ConversationReceiptModeUpdate
  = ConversationReceiptModeUpdate
      { cruReceiptMode :: !ReceiptMode
      }
  deriving (Eq, Show)

data ConversationMessageTimerUpdate
  = ConversationMessageTimerUpdate
      { -- | New message timer
        cupMessageTimer :: !(Maybe Milliseconds)
      }
  deriving (Eq, Show)

data ConvTeamInfo
  = ConvTeamInfo
      { cnvTeamId :: !TeamId,
        cnvManaged :: !Bool
      }
  deriving (Eq, Show)

data NewConv
  = NewConv
      { newConvUsers :: ![OpaqueUserId],
        newConvName :: !(Maybe Text),
        newConvAccess :: !(Set Access),
        newConvAccessRole :: !(Maybe AccessRole),
        newConvTeam :: !(Maybe ConvTeamInfo),
        newConvMessageTimer :: !(Maybe Milliseconds),
        newConvReceiptMode :: !(Maybe ReceiptMode),
        newConvUsersRole :: !RoleName
        -- Every member except for the creator will have this role
      }

deriving instance Eq NewConv

deriving instance Show NewConv

newtype NewConvManaged = NewConvManaged NewConv
  deriving (Eq, Show)

newtype NewConvUnmanaged = NewConvUnmanaged NewConv
  deriving (Eq, Show)

{- Note [managed conversations]
~~~~~~~~~~~~~~~~~~~~~~

Managed conversations are conversations where every team member is present
automatically. They have been implemented on the backend but never used in
production, and as of July 2, 2018 no managed conversations exist "in the
wild". They also prevent us from decoupling team size and conversation size
-- by essentially demanding that they be equal, while in reality allowing
huge teams is much easier than allowing huge conversations and we want to
use that fact.

For the reason above, it's been decided to remove support for creating
managed conversations from the backend. However, we are not 100% sure that
we won't introduce them again in the future, and so we'd like to retain all
the logic and tests that we have now.

To that end we have the following types:

  * data NewConv -- allows both managed and unmanaged conversations;
  * newtype NewConvUnmanaged -- only unmanaged;
  * newtype NewConvManaged -- only managed.

Those are invariants enforced on the 'FromJSON' level. For convenience, the
newtype constructors have not been hidden.

The public POST /conversations endpoint only allows unmanaged conversations.
For creating managed conversations we provide an internal endpoint called
POST /i/conversations/managed. When an endpoint receives payload
corresponding to a forbidden conversation type, it throws a JSON parsing
error, which is not optimal but it doesn't matter since nobody is trying to
create managed conversations anyway.
-}

newtype UserClientMap a
  = UserClientMap
      { userClientMap :: Map OpaqueUserId (Map ClientId a)
      }
  deriving
    ( Eq,
      Show,
      Functor,
      Foldable,
      Semigroup,
      Monoid,
      Traversable
    )

newtype OtrRecipients
  = OtrRecipients
      { otrRecipientsMap :: UserClientMap Text
      }
  deriving
    ( Eq,
      Show,
      ToJSON,
      FromJSON,
      Semigroup,
      Monoid
    )

foldrOtrRecipients :: (OpaqueUserId -> ClientId -> Text -> a -> a) -> a -> OtrRecipients -> a
foldrOtrRecipients f a =
  Map.foldrWithKey go a
    . userClientMap
    . otrRecipientsMap
  where
    go u cs acc = Map.foldrWithKey (f u) acc cs

-- | A setting for choosing what to do when a message has not been encrypted
-- for all recipients.
data OtrFilterMissing
  = -- | Pretend everything is okay
    OtrIgnoreAllMissing
  | -- | Complain (default)
    OtrReportAllMissing
  | -- | Complain only about missing
    --      recipients who are /not/ on this list
    OtrIgnoreMissing (Set OpaqueUserId)
  | -- | Complain only about missing
    --      recipients who /are/ on this list
    OtrReportMissing (Set OpaqueUserId)

data NewOtrMessage
  = NewOtrMessage
      { newOtrSender :: !ClientId,
        newOtrRecipients :: !OtrRecipients,
        newOtrNativePush :: !Bool,
        newOtrTransient :: !Bool,
        newOtrNativePriority :: !(Maybe Priority),
        newOtrData :: !(Maybe Text),
        newOtrReportMissing :: !(Maybe [OpaqueUserId])
        -- FUTUREWORK: if (and only if) clients can promise this uid list will always exactly
        -- be the list of uids we could also extract from the messages' recipients field, we
        -- should do the latter, for two reasons: (1) no need for an artificial limit on the
        -- body field length, because it'd be just a boolean; (2) less network consumption.
      }

newtype UserClients
  = UserClients
      { userClients :: Map OpaqueUserId (Set ClientId)
      }
  deriving (Eq, Show, Semigroup, Monoid, Generic)

filterClients :: (Set ClientId -> Bool) -> UserClients -> UserClients
filterClients p (UserClients c) = UserClients $ Map.filter p c

data ClientMismatch
  = ClientMismatch
      { cmismatchTime :: !UTCTime,
        -- | Clients that the message /should/ have been encrypted for, but wasn't.
        missingClients :: !UserClients,
        -- | Clients that the message /should not/ have been encrypted for, but was.
        redundantClients :: !UserClients,
        deletedClients :: !UserClients
      }
  deriving (Eq, Show, Generic)

-- | Request payload for accepting a 1-1 conversation.
newtype Accept
  = Accept
      { aUser :: UserId
      }
  deriving (Eq, Show, Generic)

-- Members ------------------------------------------------------------------

-- The semantics of the possible different values is entirely up to clients,
-- the server will not interpret this value in any way.
newtype MutedStatus = MutedStatus {fromMutedStatus :: Int32}
  deriving (Eq, Num, Ord, Show, FromJSON, ToJSON, Generic)

data SimpleMember
  = SimpleMember
      { smId :: !OpaqueUserId,
        smConvRoleName :: !RoleName
      }
  deriving (Eq, Show, Generic)

data Member
  = Member
      { memId :: !OpaqueUserId,
        memService :: !(Maybe ServiceRef),
        -- | DEPRECATED, remove it once enough clients use `memOtrMutedStatus`
        memOtrMuted :: !Bool,
        memOtrMutedStatus :: !(Maybe MutedStatus),
        memOtrMutedRef :: !(Maybe Text),
        memOtrArchived :: !Bool,
        memOtrArchivedRef :: !(Maybe Text),
        memHidden :: !Bool,
        memHiddenRef :: !(Maybe Text),
        memConvRoleName :: !RoleName
      }
  deriving (Eq, Show, Generic)

data OtherMember
  = OtherMember
      { omId :: !OpaqueUserId,
        omService :: !(Maybe ServiceRef),
        omConvRoleName :: !RoleName
      }
  deriving (Eq, Show, Generic)

instance Ord OtherMember where
  compare a b = compare (omId a) (omId b)

-- Inbound self member updates.  This is what galley expects on its endpoint.  See also
-- 'MemberUpdateData' - that event is meant to be sent only to the _self_ user.
data MemberUpdate
  = MemberUpdate
      { mupOtrMute :: !(Maybe Bool),
        mupOtrMuteStatus :: !(Maybe MutedStatus),
        mupOtrMuteRef :: !(Maybe Text),
        mupOtrArchive :: !(Maybe Bool),
        mupOtrArchiveRef :: !(Maybe Text),
        mupHidden :: !(Maybe Bool),
        mupHiddenRef :: !(Maybe Text),
        mupConvRoleName :: !(Maybe RoleName)
      }

memberUpdate :: MemberUpdate
memberUpdate = MemberUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

deriving instance Eq MemberUpdate

deriving instance Show MemberUpdate

-- Inbound other member updates.  This is what galley expects on its endpoint.  See also
-- 'OtherMemberUpdateData' - that event is meant to be sent to all users in a conversation.
data OtherMemberUpdate
  = OtherMemberUpdate
      { omuConvRoleName :: !(Maybe RoleName)
      }

deriving instance Eq OtherMemberUpdate

deriving instance Show OtherMemberUpdate

data Invite
  = Invite
      { invUsers :: !(List1 OpaqueUserId),
        invRoleName :: !RoleName -- This role name is to be applied to all users
      }

newInvite :: List1 OpaqueUserId -> Invite
newInvite us = Invite us roleNameWireAdmin

deriving instance Eq Invite

deriving instance Show Invite

-- Events -------------------------------------------------------------------

-- FUTUREWORK(federation, #1213):
-- Conversation and user ID can be remote IDs, but the receiver might be on
-- another backend, so mapped IDs don't work for them.
data Event
  = Event
      { evtType :: !EventType,
        evtConv :: !OpaqueConvId,
        evtFrom :: !OpaqueUserId,
        evtTime :: !UTCTime,
        evtData :: !(Maybe EventData)
      }
  deriving (Eq, Generic)

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
  deriving (Eq, Show, Generic)

-- FUTUREWORK(federation, #1213):
-- A lot of information in the events can contain remote IDs, but the
-- receiver might be on another backend, so mapped IDs don't work for them.
data EventData
  = EdMembersJoin !SimpleMembers
  | EdMembersLeave !OpaqueUserIdList
  | EdConnect !Connect
  | EdConvReceiptModeUpdate !ConversationReceiptModeUpdate
  | EdConvRename !ConversationRename
  | EdConvAccessUpdate !ConversationAccessUpdate
  | EdConvMessageTimerUpdate !ConversationMessageTimerUpdate
  | EdConvCodeUpdate !ConversationCode
  | EdMemberUpdate !MemberUpdateData
  | EdConversation !Conversation
  | EdTyping !TypingData
  | EdOtrMessage !OtrMessage
  deriving (Eq, Show, Generic)

data OtrMessage
  = OtrMessage
      { otrSender :: !ClientId,
        otrRecipient :: !ClientId,
        otrCiphertext :: !Text,
        otrData :: !(Maybe Text)
      }
  deriving (Eq, Show, Generic)

newtype SimpleMembers
  = SimpleMembers
      { mMembers :: [SimpleMember]
      }
  deriving (Eq, Show, Generic)

-- This datatype replaces the old `Members` datatype,
-- which has been replaced by `SimpleMembers`. This is
-- needed due to backwards compatible reasons since old
-- clients will break if we switch these types. Also, this
-- definition represents better what information it carries
newtype UserIdList
  = UserIdList
      { uilUsers :: [UserId]
      }
  deriving (Eq, Show, Generic)

newtype OpaqueUserIdList
  = OpaqueUserIdList
      { ouilUsers :: [OpaqueUserId]
      }
  deriving (Eq, Show, Generic)

data Connect
  = Connect
      { -- | There is no clear plan for connections across backends
        -- yet, so we leave this user Id local for now.
        cRecipient :: !UserId,
        cMessage :: !(Maybe Text),
        cName :: !(Maybe Text),
        cEmail :: !(Maybe Text)
      }
  deriving (Eq, Show, Generic)

-- Outbound member updates. When a user A acts upon a user B,
-- then a user event is generated where B's user ID is set
-- as misTarget.
-- Used for events (sent over the websocket, etc.).  See also
-- 'MemberUpdate' and 'OtherMemberUpdate'.
data MemberUpdateData
  = MemberUpdateData
      { misTarget :: !(Maybe OpaqueUserId), -- Target user of this action
        misOtrMuted :: !(Maybe Bool),
        misOtrMutedStatus :: !(Maybe MutedStatus),
        misOtrMutedRef :: !(Maybe Text),
        misOtrArchived :: !(Maybe Bool),
        misOtrArchivedRef :: !(Maybe Text),
        misHidden :: !(Maybe Bool),
        misHiddenRef :: !(Maybe Text),
        misConvRoleName :: !(Maybe RoleName)
      }
  deriving (Eq, Show, Generic)

newtype TypingData
  = TypingData
      { tdStatus :: TypingStatus
      }
  deriving (Eq, Show, Generic)

data TypingStatus
  = StartedTyping
  | StoppedTyping
  deriving (Eq, Ord, Show, Generic)

data ConversationCode
  = ConversationCode
      { conversationKey :: !Code.Key,
        conversationCode :: !Code.Value,
        conversationUri :: !(Maybe HttpsUrl)
      }
  deriving (Eq, Show, Generic)

mkConversationCode :: Code.Key -> Code.Value -> HttpsUrl -> ConversationCode
mkConversationCode k v (HttpsUrl prefix) =
  ConversationCode
    { conversationKey = k,
      conversationCode = v,
      conversationUri = Just (HttpsUrl link)
    }
  where
    q = [("key", toByteString' k), ("code", toByteString' v)]
    link = prefix & (queryL . queryPairsL) .~ q

data CustomBackend
  = CustomBackend
      { backendConfigJsonUrl :: !HttpsUrl,
        backendWebappWelcomeUrl :: !HttpsUrl
      }
  deriving (Eq, Show)

-- Instances ----------------------------------------------------------------

-- JSON

instance ToJSON Access where
  toJSON PrivateAccess = String "private"
  toJSON InviteAccess = String "invite"
  toJSON LinkAccess = String "link"
  toJSON CodeAccess = String "code"

instance FromJSON Access where
  parseJSON = withText "Access" $ \s ->
    case s of
      "private" -> return PrivateAccess
      "invite" -> return InviteAccess
      "link" -> return LinkAccess
      "code" -> return CodeAccess
      x -> fail ("Invalid Access Mode: " ++ show x)

instance FromJSON AccessRole where
  parseJSON = withText "access-role" $ \s ->
    case s of
      "private" -> return PrivateAccessRole
      "team" -> return TeamAccessRole
      "activated" -> return ActivatedAccessRole
      "non_activated" -> return NonActivatedAccessRole
      x -> fail ("Invalid Access Role: " ++ show x)

instance FromJSON ReceiptMode where
  parseJSON x = ReceiptMode <$> parseJSON x

instance ToJSON ReceiptMode where
  toJSON = toJSON . unReceiptMode

instance ToJSON AccessRole where
  toJSON PrivateAccessRole = String "private"
  toJSON TeamAccessRole = String "team"
  toJSON ActivatedAccessRole = String "activated"
  toJSON NonActivatedAccessRole = String "non_activated"

instance ToJSON UserClients where
  toJSON =
    toJSON . Map.foldrWithKey' fn Map.empty . userClients
    where
      fn u c m =
        let k = T.decodeLatin1 (toASCIIBytes (toUUID u))
         in Map.insert k c m

instance FromJSON UserClients where
  parseJSON =
    withObject "UserClients" (fmap UserClients . foldrM fn Map.empty . HashMap.toList)
    where
      fn (k, v) m = Map.insert <$> parseJSON (String k) <*> parseJSON v <*> pure m

instance ToJSON ClientMismatch where
  toJSON m =
    object
      [ "time" .= toUTCTimeMillis (cmismatchTime m),
        "missing" .= missingClients m,
        "redundant" .= redundantClients m,
        "deleted" .= deletedClients m
      ]

instance FromJSON ClientMismatch where
  parseJSON = withObject "ClientMismatch" $ \o ->
    ClientMismatch <$> o .: "time"
      <*> o .: "missing"
      <*> o .: "redundant"
      <*> o .: "deleted"

instance ToJSON OtrMessage where
  toJSON m =
    object $
      "sender" .= otrSender m
        # "recipient" .= otrRecipient m
        # "text" .= otrCiphertext m
        # "data" .= otrData m
        # []

instance FromJSON OtrMessage where
  parseJSON = withObject "otr-message" $ \o ->
    OtrMessage <$> o .: "sender"
      <*> o .: "recipient"
      <*> o .: "text"
      <*> o .:? "data"

instance ToJSON a => ToJSON (UserClientMap a) where
  toJSON = toJSON . Map.foldrWithKey' f Map.empty . userClientMap
    where
      f (Id u) clients m =
        let key = T.decodeLatin1 (toASCIIBytes u)
            val = Map.foldrWithKey' g Map.empty clients
         in Map.insert key val m
      g (ClientId c) a = Map.insert c (toJSON a)

instance FromJSON a => FromJSON (UserClientMap a) where
  parseJSON = withObject "user-client-map" $ \o ->
    UserClientMap <$> foldrM f Map.empty (HashMap.toList o)
    where
      f (k, v) m = do
        u <- parseJSON (String k)
        flip (withObject "client-value-map") v $ \c -> do
          e <- foldrM g Map.empty (HashMap.toList c)
          return (Map.insert u e m)
      g (k, v) m = do
        c <- parseJSON (String k)
        t <- parseJSON v
        return (Map.insert c t m)

instance ToJSON NewOtrMessage where
  toJSON otr =
    object $
      "sender" .= newOtrSender otr
        # "recipients" .= newOtrRecipients otr
        # "native_push" .= newOtrNativePush otr
        # "transient" .= newOtrTransient otr
        # "native_priority" .= newOtrNativePriority otr
        # "data" .= newOtrData otr
        # "report_missing" .= newOtrReportMissing otr
        # []

instance FromJSON NewOtrMessage where
  parseJSON = withObject "new-otr-message" $ \o ->
    NewOtrMessage <$> o .: "sender"
      <*> o .: "recipients"
      <*> o .:? "native_push" .!= True
      <*> o .:? "transient" .!= False
      <*> o .:? "native_priority"
      <*> o .:? "data"
      <*> o .:? "report_missing"

instance FromJSON Accept where
  parseJSON = withObject "accept" $ \o ->
    Accept <$> o .: "user"

instance ToJSON Accept where
  toJSON a =
    object
      [ "user" .= aUser a
      ]

instance ToJSON OtherMember where
  toJSON m =
    object $
      "id" .= omId m
        # "status" .= (0 :: Int) -- TODO: Remove
        # "service" .= omService m
        # "conversation_role" .= omConvRoleName m
        # []

instance FromJSON OtherMember where
  parseJSON = withObject "other-member" $ \o ->
    OtherMember <$> o .: "id"
      <*> o .:? "service"
      <*> o .:? "conversation_role" .!= roleNameWireAdmin

instance ToJSON a => ToJSON (ConversationList a) where
  toJSON (ConversationList l m) =
    object
      [ "conversations" .= l,
        "has_more" .= m
      ]

instance FromJSON a => FromJSON (ConversationList a) where
  parseJSON = withObject "conversation-list" $ \o ->
    ConversationList <$> o .: "conversations"
      <*> o .: "has_more"

instance ToJSON Conversation where
  toJSON c =
    object
      [ "id" .= cnvId c,
        "type" .= cnvType c,
        "creator" .= cnvCreator c,
        "access" .= cnvAccess c,
        "access_role" .= cnvAccessRole c,
        "name" .= cnvName c,
        "members" .= cnvMembers c,
        "last_event" .= ("0.0" :: Text),
        "last_event_time" .= ("1970-01-01T00:00:00.000Z" :: Text),
        "team" .= cnvTeam c,
        "message_timer" .= cnvMessageTimer c,
        "receipt_mode" .= cnvReceiptMode c
      ]

instance FromJSON Conversation where
  parseJSON = withObject "conversation" $ \o ->
    Conversation <$> o .: "id"
      <*> o .: "type"
      <*> o .: "creator"
      <*> o .: "access"
      <*> o .:? "access_role" .!= ActivatedAccessRole
      <*> o .:? "name"
      <*> o .: "members"
      <*> o .:? "team"
      <*> o .:? "message_timer"
      <*> o .:? "receipt_mode"

instance ToJSON ConvMembers where
  toJSON mm =
    object
      [ "self" .= cmSelf mm,
        "others" .= cmOthers mm
      ]

instance FromJSON ConvMembers where
  parseJSON =
    withObject
      "conv-members"
      ( \o ->
          ConvMembers <$> o .: "self"
            <*> o .: "others"
      )

instance FromJSON Event where
  parseJSON = withObject "event" $ \o -> do
    t <- o .: "type"
    d <- o .: "data"
    Event t <$> o .: "conversation"
      <*> o .: "from"
      <*> o .: "time"
      <*> parseEventData t d

parseEventData :: EventType -> Value -> Parser (Maybe EventData)
parseEventData MemberJoin v = Just . EdMembersJoin <$> parseJSON v
parseEventData MemberLeave v = Just . EdMembersLeave <$> parseJSON v
parseEventData MemberStateUpdate v = Just . EdMemberUpdate <$> parseJSON v
parseEventData ConvRename v = Just . EdConvRename <$> parseJSON v
parseEventData ConvAccessUpdate v = Just . EdConvAccessUpdate <$> parseJSON v
parseEventData ConvMessageTimerUpdate v = Just . EdConvMessageTimerUpdate <$> parseJSON v
parseEventData ConvCodeUpdate v = Just . EdConvCodeUpdate <$> parseJSON v
parseEventData ConvCodeDelete _ = pure Nothing
parseEventData ConvConnect v = Just . EdConnect <$> parseJSON v
parseEventData ConvCreate v = Just . EdConversation <$> parseJSON v
parseEventData ConvReceiptModeUpdate v = Just . EdConvReceiptModeUpdate <$> parseJSON v
parseEventData Typing v = Just . EdTyping <$> parseJSON v
parseEventData OtrMessageAdd v = Just . EdOtrMessage <$> parseJSON v
parseEventData ConvDelete _ = pure Nothing

instance ToJSON EventData where
  toJSON (EdMembersJoin x) = toJSON x
  toJSON (EdMembersLeave x) = toJSON x
  toJSON (EdConnect x) = toJSON x
  toJSON (EdConvRename x) = toJSON x
  toJSON (EdConvAccessUpdate x) = toJSON x
  toJSON (EdConvMessageTimerUpdate x) = toJSON x
  toJSON (EdConvCodeUpdate x) = toJSON x
  toJSON (EdConvReceiptModeUpdate x) = toJSON x
  toJSON (EdMemberUpdate x) = toJSON x
  toJSON (EdConversation x) = toJSON x
  toJSON (EdTyping x) = toJSON x
  toJSON (EdOtrMessage x) = toJSON x

instance ToJSONObject Event where
  toJSONObject e =
    HashMap.fromList
      [ "type" .= evtType e,
        "conversation" .= evtConv e,
        "from" .= evtFrom e,
        "time" .= toUTCTimeMillis (evtTime e),
        "data" .= evtData e
      ]

instance ToJSON Event where
  toJSON = Object . toJSONObject

instance FromJSON EventType where
  parseJSON (String "conversation.member-join") = return MemberJoin
  parseJSON (String "conversation.member-leave") = return MemberLeave
  parseJSON (String "conversation.rename") = return ConvRename
  parseJSON (String "conversation.access-update") = return ConvAccessUpdate
  parseJSON (String "conversation.message-timer-update") = return ConvMessageTimerUpdate
  parseJSON (String "conversation.code-update") = return ConvCodeUpdate
  parseJSON (String "conversation.code-delete") = return ConvCodeDelete
  parseJSON (String "conversation.member-update") = return MemberStateUpdate
  parseJSON (String "conversation.create") = return ConvCreate
  parseJSON (String "conversation.delete") = return ConvDelete
  parseJSON (String "conversation.connect-request") = return ConvConnect
  parseJSON (String "conversation.receipt-mode-update") = return ConvReceiptModeUpdate
  parseJSON (String "conversation.typing") = return Typing
  parseJSON (String "conversation.otr-message-add") = return OtrMessageAdd
  parseJSON x = fail $ "No event-type: " <> show (encode x)

instance ToJSON EventType where
  toJSON MemberJoin = String "conversation.member-join"
  toJSON MemberLeave = String "conversation.member-leave"
  toJSON MemberStateUpdate = String "conversation.member-update"
  toJSON ConvRename = String "conversation.rename"
  toJSON ConvAccessUpdate = String "conversation.access-update"
  toJSON ConvMessageTimerUpdate = String "conversation.message-timer-update"
  toJSON ConvCodeUpdate = String "conversation.code-update"
  toJSON ConvCodeDelete = String "conversation.code-delete"
  toJSON ConvCreate = String "conversation.create"
  toJSON ConvDelete = String "conversation.delete"
  toJSON ConvConnect = String "conversation.connect-request"
  toJSON ConvReceiptModeUpdate = String "conversation.receipt-mode-update"
  toJSON Typing = String "conversation.typing"
  toJSON OtrMessageAdd = String "conversation.otr-message-add"

newConvParseJSON :: Value -> Parser NewConv
newConvParseJSON = withObject "new-conv object" $ \i ->
  NewConv <$> i .: "users"
    <*> i .:? "name"
    <*> i .:? "access" .!= mempty
    <*> i .:? "access_role"
    <*> i .:? "team"
    <*> i .:? "message_timer"
    <*> i .:? "receipt_mode"
    <*> i .:? "conversation_role" .!= roleNameWireAdmin

newConvToJSON :: NewConv -> Value
newConvToJSON i =
  object $
    "users" .= newConvUsers i
      # "name" .= newConvName i
      # "access" .= newConvAccess i
      # "access_role" .= newConvAccessRole i
      # "team" .= newConvTeam i
      # "message_timer" .= newConvMessageTimer i
      # "receipt_mode" .= newConvReceiptMode i
      # "conversation_role" .= newConvUsersRole i
      # []

instance ToJSON NewConvUnmanaged where
  toJSON (NewConvUnmanaged nc) = newConvToJSON nc

instance ToJSON NewConvManaged where
  toJSON (NewConvManaged nc) = newConvToJSON nc

instance FromJSON NewConvUnmanaged where
  parseJSON v = do
    nc <- newConvParseJSON v
    when (maybe False cnvManaged (newConvTeam nc)) $
      fail "managed conversations have been deprecated"
    pure (NewConvUnmanaged nc)

instance FromJSON NewConvManaged where
  parseJSON v = do
    nc <- newConvParseJSON v
    unless (maybe False cnvManaged (newConvTeam nc)) $
      fail "only managed conversations are allowed here"
    pure (NewConvManaged nc)

instance ToJSON ConvTeamInfo where
  toJSON c =
    object
      [ "teamid" .= cnvTeamId c,
        "managed" .= cnvManaged c
      ]

instance FromJSON ConvTeamInfo where
  parseJSON = withObject "conversation team info" $ \o ->
    ConvTeamInfo <$> o .: "teamid" <*> o .:? "managed" .!= False

instance FromJSON Invite where
  parseJSON = withObject "invite object" $ \o ->
    Invite <$> o .: "users" <*> o .:? "conversation_role" .!= roleNameWireAdmin

instance ToJSON Invite where
  toJSON i =
    object
      [ "users" .= invUsers i,
        "conversation_role" .= invRoleName i
      ]

instance FromJSON ConversationMeta where
  parseJSON = withObject "conversation-meta" $ \o ->
    ConversationMeta <$> o .: "id"
      <*> o .: "type"
      <*> o .: "creator"
      <*> o .: "access"
      <*> o .: "access_role"
      <*> o .: "name"
      <*> o .:? "team"
      <*> o .:? "message_timer"
      <*> o .:? "receipt_mode"

instance ToJSON ConversationMeta where
  toJSON c =
    object $
      "id" .= cmId c
        # "type" .= cmType c
        # "creator" .= cmCreator c
        # "access" .= cmAccess c
        # "access_role" .= cmAccessRole c
        # "name" .= cmName c
        # "team" .= cmTeam c
        # "message_timer" .= cmMessageTimer c
        # "receipt_mode" .= cmReceiptMode c
        # []

instance ToJSON ConversationAccessUpdate where
  toJSON c =
    object $
      "access" .= cupAccess c
        # "access_role" .= cupAccessRole c
        # []

instance FromJSON ConversationAccessUpdate where
  parseJSON = withObject "conversation-access-update" $ \o ->
    ConversationAccessUpdate <$> o .: "access"
      <*> o .: "access_role"

instance FromJSON ConversationReceiptModeUpdate where
  parseJSON = withObject "conversation-receipt-mode-update" $ \o ->
    ConversationReceiptModeUpdate <$> o .: "receipt_mode"

instance ToJSON ConversationReceiptModeUpdate where
  toJSON c =
    object
      [ "receipt_mode" .= cruReceiptMode c
      ]

instance ToJSON ConversationMessageTimerUpdate where
  toJSON c =
    object
      [ "message_timer" .= cupMessageTimer c
      ]

instance FromJSON ConversationMessageTimerUpdate where
  parseJSON = withObject "conversation-message-timer-update" $ \o ->
    ConversationMessageTimerUpdate <$> o .:? "message_timer"

instance FromJSON ConversationRename where
  parseJSON = withObject "conversation-rename object" $ \c ->
    ConversationRename <$> c .: "name"

instance ToJSON ConversationRename where
  toJSON cu = object ["name" .= cupName cu]

instance FromJSON MemberUpdate where
  parseJSON = withObject "member-update object" $ \m -> do
    u <-
      MemberUpdate <$> m .:? "otr_muted"
        <*> m .:? "otr_muted_status"
        <*> m .:? "otr_muted_ref"
        <*> m .:? "otr_archived"
        <*> m .:? "otr_archived_ref"
        <*> m .:? "hidden"
        <*> m .:? "hidden_ref"
        <*> m .:? "conversation_role"
    unless
      ( isJust (mupOtrMute u)
          || isJust (mupOtrMuteStatus u)
          || isJust (mupOtrMuteRef u)
          || isJust (mupOtrArchive u)
          || isJust (mupOtrArchiveRef u)
          || isJust (mupHidden u)
          || isJust (mupHiddenRef u)
          || isJust (mupConvRoleName u)
      )
      $ fail
        "One of { \'otr_muted', 'otr_muted_ref', 'otr_archived', \
        \'otr_archived_ref', 'hidden', 'hidden_ref', 'conversation_role'} required."
    return u

instance ToJSON MemberUpdate where
  toJSON m =
    object $
      "otr_muted" .= mupOtrMute m
        # "otr_muted_ref" .= mupOtrMuteRef m
        # "otr_archived" .= mupOtrArchive m
        # "otr_archived_ref" .= mupOtrArchiveRef m
        # "hidden" .= mupHidden m
        # "hidden_ref" .= mupHiddenRef m
        # "conversation_role" .= mupConvRoleName m
        # []

instance FromJSON OtherMemberUpdate where
  parseJSON = withObject "other-member-update object" $ \m -> do
    u <- OtherMemberUpdate <$> m .:? "conversation_role"
    unless (isJust (omuConvRoleName u)) $
      fail "One of { 'conversation_role'} required."
    return u

instance ToJSON OtherMemberUpdate where
  toJSON m =
    object $
      "conversation_role" .= omuConvRoleName m
        # []

instance FromJSON MemberUpdateData where
  parseJSON = withObject "member-update event data" $ \m ->
    MemberUpdateData <$> m .:? "target"
      -- NOTE: ^-- This is really not a maybe and should
      --       be made compulsory 28 days after the next
      --       release to prod to guaratee that no events
      --       out there do not contain id.
      --       Making it compulsory now creates a bit of
      --       a fragile parser
      <*> m .:? "otr_muted"
      <*> m .:? "otr_muted_status"
      <*> m .:? "otr_muted_ref"
      <*> m .:? "otr_archived"
      <*> m .:? "otr_archived_ref"
      <*> m .:? "hidden"
      <*> m .:? "hidden_ref"
      <*> m .:? "conversation_role"

instance ToJSON MemberUpdateData where
  toJSON m =
    object $
      "target" .= misTarget m
        # "otr_muted" .= misOtrMuted m
        # "otr_muted_status" .= misOtrMutedStatus m
        # "otr_muted_ref" .= misOtrMutedRef m
        # "otr_archived" .= misOtrArchived m
        # "otr_archived_ref" .= misOtrArchivedRef m
        # "hidden" .= misHidden m
        # "hidden_ref" .= misHiddenRef m
        # "conversation_role" .= misConvRoleName m
        # []

instance ToJSON Member where
  toJSON m =
    object
      [ "id" .= memId m,
        "service" .= memService m,
        -- Remove ...
        "status" .= (0 :: Int),
        "status_ref" .= ("0.0" :: Text),
        "status_time" .= ("1970-01-01T00:00:00.000Z" :: Text),
        -- ... until here

        "otr_muted" .= memOtrMuted m,
        "otr_muted_status" .= memOtrMutedStatus m,
        "otr_muted_ref" .= memOtrMutedRef m,
        "otr_archived" .= memOtrArchived m,
        "otr_archived_ref" .= memOtrArchivedRef m,
        "hidden" .= memHidden m,
        "hidden_ref" .= memHiddenRef m,
        "conversation_role" .= memConvRoleName m
      ]

instance ToJSON SimpleMember where
  toJSON m =
    object
      [ "id" .= smId m,
        "conversation_role" .= smConvRoleName m
      ]

instance FromJSON Member where
  parseJSON = withObject "member object" $ \o ->
    Member <$> o .: "id"
      <*> o .:? "service"
      <*> o .:? "otr_muted" .!= False
      <*> o .:? "otr_muted_status"
      <*> o .:? "otr_muted_ref"
      <*> o .:? "otr_archived" .!= False
      <*> o .:? "otr_archived_ref"
      <*> o .:? "hidden" .!= False
      <*> o .:? "hidden_ref"
      <*> o .:? "conversation_role" .!= roleNameWireAdmin

instance FromJSON SimpleMember where
  parseJSON = withObject "simple member object" $ \o ->
    SimpleMember <$> o .: "id"
      <*> o .:? "conversation_role" .!= roleNameWireAdmin

instance FromJSON ConvType where
  parseJSON (Number 0) = return RegularConv
  parseJSON (Number 1) = return SelfConv
  parseJSON (Number 2) = return One2OneConv
  parseJSON (Number 3) = return ConnectConv
  parseJSON x = fail $ "No conversation-type: " <> show (encode x)

instance ToJSON ConvType where
  toJSON RegularConv = Number 0
  toJSON SelfConv = Number 1
  toJSON One2OneConv = Number 2
  toJSON ConnectConv = Number 3

instance FromJSON UserIdList where
  parseJSON = withObject "user-ids-payload" $ \o ->
    UserIdList <$> o .: "user_ids"

instance ToJSON UserIdList where
  toJSON e = object ["user_ids" .= uilUsers e]

instance FromJSON OpaqueUserIdList where
  parseJSON = withObject "user-ids-payload" $ \o ->
    OpaqueUserIdList <$> o .: "user_ids"

instance ToJSON OpaqueUserIdList where
  toJSON e = object ["user_ids" .= ouilUsers e]

instance FromJSON SimpleMembers where
  parseJSON = withObject "simple-members-payload" $ \o -> do
    users <- o .:? "users" -- This is to make migration easier and not dependent on deployment ordering
    membs <- case users of
      Just mems -> pure mems
      Nothing -> do
        ids <- o .:? "user_ids"
        case ids of
          Just userIds -> pure $ fmap (\u -> SimpleMember u roleNameWireAdmin) userIds
          Nothing -> fail "Not possible!"
    pure $ SimpleMembers membs

instance ToJSON SimpleMembers where
  toJSON e =
    object
      [ "user_ids" .= fmap smId (mMembers e),
        "users" .= mMembers e
      ]

instance FromJSON Connect where
  parseJSON = withObject "connect" $ \o ->
    Connect <$> o .: "recipient"
      <*> o .:? "message"
      <*> o .:? "name"
      <*> o .:? "email"

instance ToJSON Connect where
  toJSON c =
    object
      [ "recipient" .= cRecipient c,
        "message" .= cMessage c,
        "name" .= cName c,
        "email" .= cEmail c
      ]

instance ToJSON TypingStatus where
  toJSON StartedTyping = String "started"
  toJSON StoppedTyping = String "stopped"

instance FromJSON TypingStatus where
  parseJSON (String "started") = return StartedTyping
  parseJSON (String "stopped") = return StoppedTyping
  parseJSON x = fail $ "No status-type: " <> show x

instance ToJSON TypingData where
  toJSON t = object ["status" .= tdStatus t]

instance FromJSON TypingData where
  parseJSON = withObject "typing-data" $ \o ->
    TypingData <$> o .: "status"

instance ToJSON ConversationCode where
  toJSON j =
    object $
      "key" .= conversationKey j
        # "code" .= conversationCode j
        # "uri" .= conversationUri j
        # []

instance FromJSON ConversationCode where
  parseJSON = withObject "join" $ \o ->
    ConversationCode <$> o .: "key"
      <*> o .: "code"
      <*> o .:? "uri"

instance ToJSON CustomBackend where
  toJSON j =
    object $
      "config_json_url" .= backendConfigJsonUrl j
        # "webapp_welcome_url" .= backendWebappWelcomeUrl j
        # []

instance FromJSON CustomBackend where
  parseJSON = withObject "CustomBackend" $ \o ->
    CustomBackend
      <$> o .: "config_json_url"
      <*> o .: "webapp_welcome_url"
