{-# LANGUAGE OverloadedStrings #-}

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
  ( foldrOtrRecipients,
    Accept (..),
    ConversationMeta (..),

    -- * re-exports
    Conversation (..),
    Member (..),
    ConvMembers (..),
    OtherMember (..),
    Connect (..),
    NewOtrMessage (..),
    ClientMismatch (..),
    OtrRecipients (..),
    OtrFilterMissing (..),
    ConvTeamInfo (..),
    ConversationCode (..),
    mkConversationCode,
    Event (..),
    EventType (..),
    EventData (..),
    UserIdList (..),
    SimpleMember (..),
    SimpleMembers (..),
    MemberUpdateData (..),
    TypingData (..),
    OtrMessage (..),
    Access (..),
    AccessRole (..),
    ConversationList (..),
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
    filterClients,
    newInvite,
    memberUpdate,
  )
where

import Data.Aeson
import Data.Id (ClientId, ConvId, OpaqueUserId, TeamId, UserId)
import Data.Json.Util ((#))
import qualified Data.Map.Strict as Map
import Data.Misc (Milliseconds)
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.User (UserIdList (..))
import Wire.API.User.Client

-- Conversations ------------------------------------------------------------

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

--------------------------------------------------------------------------------

foldrOtrRecipients :: (OpaqueUserId -> ClientId -> Text -> a -> a) -> a -> OtrRecipients -> a
foldrOtrRecipients f a =
  Map.foldrWithKey go a
    . userClientMap
    . otrRecipientsMap
  where
    go u cs acc = Map.foldrWithKey (f u) acc cs

-- | Request payload for accepting a 1-1 conversation.
newtype Accept = Accept
  { aUser :: UserId
  }
  deriving (Eq, Show, Generic)

-- Instances ----------------------------------------------------------------

instance FromJSON Accept where
  parseJSON = withObject "accept" $ \o ->
    Accept <$> o .: "user"

instance ToJSON Accept where
  toJSON a =
    object
      [ "user" .= aUser a
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
