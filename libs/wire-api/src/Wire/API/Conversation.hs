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
  ( -- * Galley conversation types
    Conversation (..),
    ConvMembers (..),
    ConvTeamInfo (..),

    -- * Other galley types
    Access (..),
    AccessRole (..),
    Accept (..),
    ConversationList (..),
    ConversationMeta (..),
    ConvType (..),
    Invite (..),
    NewConv (..),
    NewConvManaged (..),
    NewConvUnmanaged (..),
    ReceiptMode (..),
    newInvite,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Id
import Data.Json.Util
import Data.List1
import Data.Misc
import Imports
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role

-- Conversations ------------------------------------------------------------

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

data ConversationMeta = ConversationMeta
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

data ConversationList a = ConversationList
  { convList :: [a],
    convHasMore :: !Bool
  }
  deriving (Eq, Show)

data ConvTeamInfo = ConvTeamInfo
  { cnvTeamId :: !TeamId,
    cnvManaged :: !Bool
  }
  deriving (Eq, Show)

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

-- | Request payload for accepting a 1-1 conversation.
newtype Accept = Accept
  { aUser :: UserId
  }
  deriving (Eq, Show, Generic)

-- Members ------------------------------------------------------------------

data Invite = Invite
  { invUsers :: !(List1 OpaqueUserId),
    -- | This role name is to be applied to all users
    invRoleName :: !RoleName
  }

newInvite :: List1 OpaqueUserId -> Invite
newInvite us = Invite us roleNameWireAdmin

deriving instance Eq Invite

deriving instance Show Invite

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

instance FromJSON Accept where
  parseJSON = withObject "accept" $ \o ->
    Accept <$> o .: "user"

instance ToJSON Accept where
  toJSON a =
    object
      [ "user" .= aUser a
      ]

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
