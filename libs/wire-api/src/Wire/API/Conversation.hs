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

-- FUTUREWORK:
-- There's still a lot of stuff we should factor out into separate modules.
module Wire.API.Conversation
  ( -- * Conversation
    Conversation (..),
    ConversationList (..),
    -- ConversationIdList TODO

    -- * Conversation properties
    Access (..),
    AccessRole (..),
    ConvType (..),
    ReceiptMode (..),

    -- * create
    NewConv (..),
    NewConvManaged (..),
    NewConvUnmanaged (..),
    ConvTeamInfo (..),

    -- * invite
    Invite (..),
    newInvite,

    -- * update
    ConversationRename (..),
    ConversationAccessUpdate (..),
    ConversationReceiptModeUpdate (..),
    ConversationMessageTimerUpdate (..),

    -- * Swagger
    modelConversation,
    modelConversations,
    modelConversationIds,
    modelInvite,
    modelNewConversation,
    modelConversationUpdateName,
    modelConversationAccessUpdate,
    modelConversationReceiptModeUpdate,
    modelConversationMessageTimerUpdate,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Id
import Data.Json.Util
import Data.List1
import Data.Misc
import Data.String.Conversions (cs)
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role (RoleName, roleNameWireAdmin)

--------------------------------------------------------------------------------
-- Conversation

-- | Public-facing conversation type. Represents information that a
-- particular user is allowed to see.
--
-- Can be produced from the internal one ('Galley.Data.Types.Conversation')
-- by using 'Galley.API.Mapping.conversationView'.
data Conversation = Conversation
  { cnvId :: !ConvId,
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

modelConversation :: Doc.Model
modelConversation = Doc.defineModel "Conversation" $ do
  Doc.description "A conversation object as returned from the server"
  Doc.property "id" Doc.bytes' $
    Doc.description "Conversation ID"
  Doc.property "type" typeConversationType $
    Doc.description "The conversation type of this object (0 = regular, 1 = self, 2 = 1:1, 3 = connect)"
  Doc.property "creator" Doc.bytes' $
    Doc.description "The creator's user ID."
  -- TODO: Doc.property "access"
  -- Doc.property "access_role"
  Doc.property "name" Doc.string' $ do
    Doc.description "The conversation name (can be null)"
  Doc.property "members" (Doc.ref modelConversationMembers) $
    Doc.description "The current set of conversation members"
  -- Doc.property "team"
  Doc.property "message_timer" (Doc.int64 (Doc.min 0)) $ do
    Doc.description "Per-conversation message timer (can be null)"

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

-- | This is used to describe a @ConversationList ConvId@.
--
-- FUTUREWORK: Create a new ConversationIdList type instead.
modelConversationIds :: Doc.Model
modelConversationIds = Doc.defineModel "ConversationIds" $ do
  Doc.description "Object holding a list of conversation IDs"
  Doc.property "conversations" (Doc.unique $ Doc.array Doc.string') Doc.end
  Doc.property "has_more" Doc.bool' $
    Doc.description "Indicator that the server has more IDs than returned"

-- | This is used to describe a @ConversationList Conversation@.
modelConversations :: Doc.Model
modelConversations = Doc.defineModel "Conversations" $ do
  Doc.description "Object holding a list of conversations"
  Doc.property "conversations" (Doc.unique $ Doc.array (Doc.ref modelConversation)) Doc.end
  Doc.property "has_more" Doc.bool' $
    Doc.description "Indicator that the server has more conversations than returned"

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

data ConversationList a = ConversationList
  { convList :: [a],
    convHasMore :: !Bool
  }
  deriving (Eq, Show)

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

--------------------------------------------------------------------------------
-- Conversation properties

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

typeAccess :: Doc.DataType
typeAccess = Doc.string . Doc.enum $ cs . encode <$> [(minBound :: Access) ..]

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

instance ToJSON AccessRole where
  toJSON PrivateAccessRole = String "private"
  toJSON TeamAccessRole = String "team"
  toJSON ActivatedAccessRole = String "activated"
  toJSON NonActivatedAccessRole = String "non_activated"

instance FromJSON AccessRole where
  parseJSON = withText "access-role" $ \s ->
    case s of
      "private" -> return PrivateAccessRole
      "team" -> return TeamAccessRole
      "activated" -> return ActivatedAccessRole
      "non_activated" -> return NonActivatedAccessRole
      x -> fail ("Invalid Access Role: " ++ show x)

data ConvType
  = RegularConv
  | SelfConv
  | One2OneConv
  | ConnectConv
  deriving (Eq, Show)

typeConversationType :: Doc.DataType
typeConversationType = Doc.int32 $ Doc.enum [0, 1, 2, 3]

instance ToJSON ConvType where
  toJSON RegularConv = Number 0
  toJSON SelfConv = Number 1
  toJSON One2OneConv = Number 2
  toJSON ConnectConv = Number 3

instance FromJSON ConvType where
  parseJSON (Number 0) = return RegularConv
  parseJSON (Number 1) = return SelfConv
  parseJSON (Number 2) = return One2OneConv
  parseJSON (Number 3) = return ConnectConv
  parseJSON x = fail $ "No conversation-type: " <> show (encode x)

-- | Define whether receipts should be sent in the given conversation
--   This datatype is defined as an int32 but the Backend does not
--   interpret it in any way, rather just stores and forwards it
--   for clients
--   E.g. of an implementation: 0 - send no ReceiptModes
--                              1 - send read ReceiptModes
--                              2 - send delivery ReceiptModes
--                              ...
newtype ReceiptMode = ReceiptMode {unReceiptMode :: Int32}
  deriving (Eq, Ord, Show)

instance ToJSON ReceiptMode where
  toJSON = toJSON . unReceiptMode

instance FromJSON ReceiptMode where
  parseJSON x = ReceiptMode <$> parseJSON x

--------------------------------------------------------------------------------
-- create

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

newtype NewConvManaged = NewConvManaged NewConv
  deriving (Eq, Show)

instance ToJSON NewConvManaged where
  toJSON (NewConvManaged nc) = newConvToJSON nc

instance FromJSON NewConvManaged where
  parseJSON v = do
    nc <- newConvParseJSON v
    unless (maybe False cnvManaged (newConvTeam nc)) $
      fail "only managed conversations are allowed here"
    pure (NewConvManaged nc)

newtype NewConvUnmanaged = NewConvUnmanaged NewConv
  deriving (Eq, Show)

-- | Used to describe a 'NewConvUnmanaged'.
modelNewConversation :: Doc.Model
modelNewConversation = Doc.defineModel "NewConversation" $ do
  Doc.description "JSON object to create a new conversation"
  Doc.property "users" (Doc.unique $ Doc.array Doc.bytes') $
    Doc.description "List of user IDs (excluding the requestor) to be part of this conversation"
  Doc.property "name" Doc.string' $ do
    Doc.description "The conversation name"
    Doc.optional
  Doc.property "team" (Doc.ref modelTeamInfo) $ do
    Doc.description "Team information of this conversation"
    Doc.optional
  -- TODO: Doc.property "access"
  -- Doc.property "access_role"
  Doc.property "message_timer" (Doc.int64 (Doc.min 0)) $ do
    Doc.description "Per-conversation message timer"
    Doc.optional
  Doc.property "receipt_mode" (Doc.int32 (Doc.min 0)) $ do
    Doc.description "Conversation receipt mode"
    Doc.optional

instance ToJSON NewConvUnmanaged where
  toJSON (NewConvUnmanaged nc) = newConvToJSON nc

instance FromJSON NewConvUnmanaged where
  parseJSON v = do
    nc <- newConvParseJSON v
    when (maybe False cnvManaged (newConvTeam nc)) $
      fail "managed conversations have been deprecated"
    pure (NewConvUnmanaged nc)

data NewConv = NewConv
  { newConvUsers :: ![OpaqueUserId],
    newConvName :: !(Maybe Text),
    newConvAccess :: !(Set Access),
    newConvAccessRole :: !(Maybe AccessRole),
    newConvTeam :: !(Maybe ConvTeamInfo),
    newConvMessageTimer :: !(Maybe Milliseconds),
    newConvReceiptMode :: !(Maybe ReceiptMode),
    -- | Every member except for the creator will have this role
    newConvUsersRole :: !RoleName
  }

deriving instance Eq NewConv

deriving instance Show NewConv

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

data ConvTeamInfo = ConvTeamInfo
  { cnvTeamId :: !TeamId,
    cnvManaged :: !Bool
  }
  deriving (Eq, Show)

modelTeamInfo :: Doc.Model
modelTeamInfo = Doc.defineModel "TeamInfo" $ do
  Doc.description "Team information"
  Doc.property "teamid" Doc.bytes' $
    Doc.description "Team ID"
  Doc.property "managed" Doc.bool' $
    Doc.description "Is this a managed team conversation?"

instance ToJSON ConvTeamInfo where
  toJSON c =
    object
      [ "teamid" .= cnvTeamId c,
        "managed" .= cnvManaged c
      ]

instance FromJSON ConvTeamInfo where
  parseJSON = withObject "conversation team info" $ \o ->
    ConvTeamInfo <$> o .: "teamid" <*> o .:? "managed" .!= False

--------------------------------------------------------------------------------
-- invite

data Invite = Invite
  { invUsers :: !(List1 OpaqueUserId),
    -- | This role name is to be applied to all users
    invRoleName :: !RoleName
  }

deriving instance Eq Invite

deriving instance Show Invite

newInvite :: List1 OpaqueUserId -> Invite
newInvite us = Invite us roleNameWireAdmin

modelInvite :: Doc.Model
modelInvite = Doc.defineModel "Invite" $ do
  Doc.description "Add users to a conversation"
  Doc.property "users" (Doc.unique $ Doc.array Doc.bytes') $
    Doc.description "List of user IDs to add to a conversation"

instance ToJSON Invite where
  toJSON i =
    object
      [ "users" .= invUsers i,
        "conversation_role" .= invRoleName i
      ]

instance FromJSON Invite where
  parseJSON = withObject "invite object" $ \o ->
    Invite <$> o .: "users" <*> o .:? "conversation_role" .!= roleNameWireAdmin

--------------------------------------------------------------------------------
-- update

newtype ConversationRename = ConversationRename
  { cupName :: Text
  }

deriving instance Eq ConversationRename

deriving instance Show ConversationRename

modelConversationUpdateName :: Doc.Model
modelConversationUpdateName = Doc.defineModel "ConversationUpdateName" $ do
  Doc.description "Contains conversation name to update"
  Doc.property "name" Doc.string' $
    Doc.description "The new conversation name"

instance ToJSON ConversationRename where
  toJSON cu = object ["name" .= cupName cu]

instance FromJSON ConversationRename where
  parseJSON = withObject "conversation-rename object" $ \c ->
    ConversationRename <$> c .: "name"

data ConversationAccessUpdate = ConversationAccessUpdate
  { cupAccess :: [Access],
    cupAccessRole :: AccessRole
  }
  deriving (Eq, Show)

modelConversationAccessUpdate :: Doc.Model
modelConversationAccessUpdate = Doc.defineModel "ConversationAccessUpdate" $ do
  Doc.description "Contains conversation properties to update"
  Doc.property "access" (Doc.unique $ Doc.array typeAccess) $
    Doc.description "List of conversation access modes."
  Doc.property "access_role" (Doc.bytes') $
    Doc.description "Conversation access role: private|team|activated|non_activated"

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

data ConversationReceiptModeUpdate = ConversationReceiptModeUpdate
  { cruReceiptMode :: !ReceiptMode
  }
  deriving (Eq, Show)

modelConversationReceiptModeUpdate :: Doc.Model
modelConversationReceiptModeUpdate = Doc.defineModel "conversationReceiptModeUpdate" $ do
  Doc.description
    "Contains conversation receipt mode to update to. Receipt mode tells \
    \clients whether certain types of receipts should be sent in the given \
    \conversation or not. How this value is interpreted is up to clients."
  Doc.property "receipt_mode" Doc.int32' $
    Doc.description "Receipt mode: int32"

instance ToJSON ConversationReceiptModeUpdate where
  toJSON c =
    object
      [ "receipt_mode" .= cruReceiptMode c
      ]

instance FromJSON ConversationReceiptModeUpdate where
  parseJSON = withObject "conversation-receipt-mode-update" $ \o ->
    ConversationReceiptModeUpdate <$> o .: "receipt_mode"

data ConversationMessageTimerUpdate = ConversationMessageTimerUpdate
  { -- | New message timer
    cupMessageTimer :: !(Maybe Milliseconds)
  }
  deriving (Eq, Show)

modelConversationMessageTimerUpdate :: Doc.Model
modelConversationMessageTimerUpdate = Doc.defineModel "ConversationMessageTimerUpdate" $ do
  Doc.description "Contains conversation properties to update"
  Doc.property "message_timer" Doc.int64' $
    Doc.description "Conversation message timer (in milliseconds); can be null"

instance ToJSON ConversationMessageTimerUpdate where
  toJSON c =
    object
      [ "message_timer" .= cupMessageTimer c
      ]

instance FromJSON ConversationMessageTimerUpdate where
  parseJSON = withObject "conversation-message-timer-update" $ \o ->
    ConversationMessageTimerUpdate <$> o .:? "message_timer"
