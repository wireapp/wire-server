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

module Wire.API.Conversation.Member
  ( ConvMembers (..),

    -- * Member
    Member (..),
    MutedStatus (..),
    OtherMember (..),

    -- * Member Update
    MemberUpdate (..),
    memberUpdate,
    OtherMemberUpdate (..),

    -- * Swagger
    modelConversationMembers,
    modelOtherMember,
    modelMember,
    modelMemberUpdate,
    modelOtherMemberUpdate,
  )
where

import Data.Aeson
import Data.Id
import Data.Json.Util
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Conversation.Role
import Wire.API.Provider.Service (ServiceRef, modelServiceRef)

data ConvMembers = ConvMembers
  { cmSelf :: !Member,
    cmOthers :: ![OtherMember]
  }
  deriving (Eq, Show)

modelConversationMembers :: Doc.Model
modelConversationMembers = Doc.defineModel "ConversationMembers" $ do
  Doc.description "Object representing users of a conversation."
  Doc.property "self" (Doc.ref modelMember) $
    Doc.description "The user ID of the requestor"
  Doc.property "others" (Doc.unique (Doc.array (Doc.ref modelOtherMember))) $
    Doc.description "All other current users of this conversation"

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

-- Members ------------------------------------------------------------------

data Member = Member
  { memId :: !UserId,
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

modelMember :: Doc.Model
modelMember = Doc.defineModel "Member" $ do
  Doc.property "id" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "otr_muted" Doc.bool' $ do
    Doc.description "Whether the conversation is muted"
    Doc.optional
  Doc.property "otr_muted_ref" Doc.bytes' $ do
    Doc.description "A reference point for (un)muting"
    Doc.optional
  Doc.property "otr_archived" Doc.bool' $ do
    Doc.description "Whether the conversation is archived"
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
  Doc.property "service" (Doc.ref modelServiceRef) $ do
    Doc.description "The reference to the owning service, if the member is a 'bot'."
    Doc.optional

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

-- | The semantics of the possible different values is entirely up to clients,
-- the server will not interpret this value in any way.
newtype MutedStatus = MutedStatus {fromMutedStatus :: Int32}
  deriving (Eq, Num, Ord, Show, FromJSON, ToJSON, Generic)

data OtherMember = OtherMember
  { omId :: !UserId,
    omService :: !(Maybe ServiceRef),
    omConvRoleName :: !RoleName
  }
  deriving (Eq, Show, Generic)

instance Ord OtherMember where
  compare a b = compare (omId a) (omId b)

modelOtherMember :: Doc.Model
modelOtherMember = Doc.defineModel "OtherMember" $ do
  Doc.property "id" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "service" (Doc.ref modelServiceRef) $ do
    Doc.description "The reference to the owning service, if the member is a 'bot'."
    Doc.optional

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

-- Member Updates -----------------------------------------------------------

-- | Inbound self member updates.  This is what galley expects on its endpoint.  See also
-- 'MemberUpdateData' - that event is meant to be sent only to the _self_ user.
data MemberUpdate = MemberUpdate
  { mupOtrMute :: !(Maybe Bool),
    mupOtrMuteStatus :: !(Maybe MutedStatus),
    mupOtrMuteRef :: !(Maybe Text),
    mupOtrArchive :: !(Maybe Bool),
    mupOtrArchiveRef :: !(Maybe Text),
    mupHidden :: !(Maybe Bool),
    mupHiddenRef :: !(Maybe Text),
    mupConvRoleName :: !(Maybe RoleName)
  }
  deriving stock (Eq, Show)

memberUpdate :: MemberUpdate
memberUpdate = MemberUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

modelMemberUpdate :: Doc.Model
modelMemberUpdate = Doc.defineModel "MemberUpdate" $ do
  Doc.description "Update user properties relative to a conversation"
  Doc.property "otr_muted" Doc.bool' $ do
    Doc.description "Whether to notify on conversation updates"
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

-- | Inbound other member updates.  This is what galley expects on its endpoint.  See also
-- 'OtherMemberUpdateData' - that event is meant to be sent to all users in a conversation.
data OtherMemberUpdate = OtherMemberUpdate
  { omuConvRoleName :: !(Maybe RoleName)
  }
  deriving stock (Eq, Show)

modelOtherMemberUpdate :: Doc.Model
modelOtherMemberUpdate = Doc.defineModel "otherMemberUpdate" $ do
  Doc.description "Update user properties of other members relative to a conversation"
  Doc.property "conversation_role" Doc.string' $ do
    Doc.description "Name of the conversation role updated to"
    Doc.optional

instance ToJSON OtherMemberUpdate where
  toJSON m =
    object $
      "conversation_role" .= omuConvRoleName m
        # []

instance FromJSON OtherMemberUpdate where
  parseJSON = withObject "other-member-update object" $ \m -> do
    u <- OtherMemberUpdate <$> m .:? "conversation_role"
    unless (isJust (omuConvRoleName u)) $
      fail "One of { 'conversation_role'} required."
    return u
