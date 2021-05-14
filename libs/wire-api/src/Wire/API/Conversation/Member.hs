{-# LANGUAGE DerivingVia #-}
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

import Control.Applicative
import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Imports
import qualified Test.QuickCheck as QC
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))
import Wire.API.Conversation.Role
import Wire.API.Provider.Service (ServiceRef, modelServiceRef)

data ConvMembers = ConvMembers
  { cmSelf :: Member,
    cmOthers :: [OtherMember]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConvMembers)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConvMembers

instance ToSchema ConvMembers where
  schema =
    objectWithDocModifier "ConvMembers" (description ?~ "Users of a conversation") $
      ConvMembers
        <$> cmSelf
          .= fieldWithDocModifier
            "self"
            (description ?~ "The user ID of the requestor")
            schema
        <*> cmOthers
          .= fieldWithDocModifier
            "others"
            (description ?~ "All other current users of this conversation")
            (array schema)

modelConversationMembers :: Doc.Model
modelConversationMembers = Doc.defineModel "ConversationMembers" $ do
  Doc.description "Object representing users of a conversation."
  Doc.property "self" (Doc.ref modelMember) $
    Doc.description "The user ID of the requestor"
  Doc.property "others" (Doc.unique (Doc.array (Doc.ref modelOtherMember))) $
    Doc.description "All other current users of this conversation"

--------------------------------------------------------------------------------
-- Members

data Member = Member
  { memId :: UserId,
    memService :: Maybe ServiceRef,
    -- | DEPRECATED, remove it once enough clients use `memOtrMutedStatus`
    memOtrMuted :: Bool,
    memOtrMutedStatus :: Maybe MutedStatus,
    memOtrMutedRef :: Maybe Text,
    memOtrArchived :: Bool,
    memOtrArchivedRef :: Maybe Text,
    memHidden :: Bool,
    memHiddenRef :: Maybe Text,
    memConvRoleName :: RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Member)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Member

instance ToSchema Member where
  schema =
    object "Member" $
      Member
        <$> memId .= field "id" schema
        <*> memService .= lax (field "service" (optWithDefault A.Null schema))
        --  Remove ...
        <* const () .= optional (field "status" (c (0 :: Int)))
        <* const () .= optional (field "status_ref" (c ("0.0" :: Text)))
        <* const ()
          .= optional
            ( field
                "status_time"
                (c ("1970-01-01T00:00:00.000Z" :: Text))
            )
        -- ... until here
        <*> memOtrMuted .= (field "otr_muted" schema <|> pure False)
        <*> memOtrMutedStatus .= lax (field "otr_muted_status" (optWithDefault A.Null schema))
        <*> memOtrMutedRef .= lax (field "otr_muted_ref" (optWithDefault A.Null schema))
        <*> memOtrArchived .= (field "otr_archived" schema <|> pure False)
        <*> memOtrArchivedRef .= lax (field "otr_archived_ref" (optWithDefault A.Null schema))
        <*> memHidden .= (field "hidden" schema <|> pure False)
        <*> memHiddenRef .= lax (field "hidden_ref" (optWithDefault A.Null schema))
        <*> memConvRoleName .= (field "conversation_role" schema <|> pure roleNameWireAdmin)
    where
      c :: ToJSON a => a -> ValueSchema SwaggerDoc ()
      c val = mkSchema mempty (const (pure ())) (const (pure (toJSON val)))

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

-- | The semantics of the possible different values is entirely up to clients,
-- the server will not interpret this value in any way.
newtype MutedStatus = MutedStatus {fromMutedStatus :: Int32}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToSchema, Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema MutedStatus

data OtherMember = OtherMember
  { omQualifiedId :: Qualified UserId,
    omService :: Maybe ServiceRef,
    omConvRoleName :: RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OtherMember)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema OtherMember

omId :: OtherMember -> UserId
omId = qUnqualified . omQualifiedId

instance ToSchema OtherMember where
  schema =
    object "OtherMember" $
      OtherMember
        <$> omQualifiedId .= field "qualified_id" schema
        <* omId .= optional (field "id" schema)
        <*> omService .= opt (fieldWithDocModifier "service" (description ?~ desc) schema)
        <*> omConvRoleName .= (field "conversation_role" schema <|> pure roleNameWireAdmin)
        <* const (0 :: Int) .= optional (fieldWithDocModifier "status" (description ?~ "deprecated") schema) -- TODO: remove
    where
      desc = "The reference to the owning service, if the member is a 'bot'."

instance Ord OtherMember where
  compare a b = compare (omQualifiedId a) (omQualifiedId b)

modelOtherMember :: Doc.Model
modelOtherMember = Doc.defineModel "OtherMember" $ do
  Doc.property "id" Doc.bytes' $
    Doc.description "User ID"
  Doc.property "service" (Doc.ref modelServiceRef) $ do
    Doc.description "The reference to the owning service, if the member is a 'bot'."
    Doc.optional

--------------------------------------------------------------------------------
-- Member Updates

-- | Inbound self member updates.  This is what galley expects on its endpoint.  See also
-- 'MemberUpdateData' - that event is meant to be sent only to the _self_ user.
data MemberUpdate = MemberUpdate
  { mupOtrMute :: Maybe Bool,
    mupOtrMuteStatus :: Maybe MutedStatus,
    mupOtrMuteRef :: Maybe Text,
    mupOtrArchive :: Maybe Bool,
    mupOtrArchiveRef :: Maybe Text,
    mupHidden :: Maybe Bool,
    mupHiddenRef :: Maybe Text,
    mupConvRoleName :: Maybe RoleName
  }
  deriving stock (Eq, Show, Generic)

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
    A.object $
      "otr_muted" A..= mupOtrMute m
        # "otr_muted_ref" A..= mupOtrMuteRef m
        # "otr_archived" A..= mupOtrArchive m
        # "otr_archived_ref" A..= mupOtrArchiveRef m
        # "hidden" A..= mupHidden m
        # "hidden_ref" A..= mupHiddenRef m
        # "conversation_role" A..= mupConvRoleName m
        # []

instance FromJSON MemberUpdate where
  parseJSON = A.withObject "member-update object" $ \m -> do
    u <-
      MemberUpdate
        <$> m A..:? "otr_muted"
        <*> m A..:? "otr_muted_status"
        <*> m A..:? "otr_muted_ref"
        <*> m A..:? "otr_archived"
        <*> m A..:? "otr_archived_ref"
        <*> m A..:? "hidden"
        <*> m A..:? "hidden_ref"
        <*> m A..:? "conversation_role"
    either fail pure $ validateMemberUpdate u

instance Arbitrary MemberUpdate where
  arbitrary =
    (removeMuteStatus . getGenericUniform <$> arbitrary)
      `QC.suchThat` (isRight . validateMemberUpdate)
    where
      removeMuteStatus mup = mup {mupOtrMuteStatus = Nothing}

validateMemberUpdate :: MemberUpdate -> Either String MemberUpdate
validateMemberUpdate u =
  if ( isJust (mupOtrMute u)
         || isJust (mupOtrMuteStatus u)
         || isJust (mupOtrMuteRef u)
         || isJust (mupOtrArchive u)
         || isJust (mupOtrArchiveRef u)
         || isJust (mupHidden u)
         || isJust (mupHiddenRef u)
         || isJust (mupConvRoleName u)
     )
    then Right u
    else
      Left
        "One of { \'otr_muted', 'otr_muted_ref', 'otr_archived', \
        \'otr_archived_ref', 'hidden', 'hidden_ref', 'conversation_role'} required."

-- | Inbound other member updates.  This is what galley expects on its endpoint.  See also
-- 'OtherMemberUpdateData' - that event is meant to be sent to all users in a conversation.
data OtherMemberUpdate = OtherMemberUpdate
  { omuConvRoleName :: Maybe RoleName
  }
  deriving stock (Eq, Show, Generic)

instance Arbitrary OtherMemberUpdate where
  arbitrary = OtherMemberUpdate . Just <$> arbitrary

modelOtherMemberUpdate :: Doc.Model
modelOtherMemberUpdate = Doc.defineModel "otherMemberUpdate" $ do
  Doc.description "Update user properties of other members relative to a conversation"
  Doc.property "conversation_role" Doc.string' $ do
    Doc.description "Name of the conversation role updated to"
    Doc.optional

instance ToJSON OtherMemberUpdate where
  toJSON m =
    A.object $
      "conversation_role" A..= omuConvRoleName m
        # []

instance FromJSON OtherMemberUpdate where
  parseJSON = A.withObject "other-member-update object" $ \m -> do
    u <- OtherMemberUpdate <$> m A..:? "conversation_role"
    unless (isJust (omuConvRoleName u)) $
      fail "One of { 'conversation_role'} required."
    return u
