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

-- FUTUREWORK:
-- There's still a lot of stuff we should factor out into separate modules.
module Wire.API.Conversation
  ( -- * Conversation
    Conversation (..),
    ConversationList (..),

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

    -- * re-exports
    module Wire.API.Conversation.Member,

    -- * Swagger
    modelConversation,
    modelConversations,
    modelConversationIds,
    modelInvite,
    modelNewConversation,
    modelTeamInfo,
    modelConversationUpdateName,
    modelConversationAccessUpdate,
    modelConversationReceiptModeUpdate,
    modelConversationMessageTimerUpdate,
    typeConversationType,
    typeAccess,
  )
where

import Control.Applicative
import Control.Lens (at, (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Id
import Data.Json.Util
import Data.List1
import Data.Misc
import Data.Proxy (Proxy (Proxy))
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import Imports
import qualified Test.QuickCheck as QC
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))
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
  { cnvId :: ConvId,
    cnvType :: ConvType,
    cnvCreator :: UserId,
    cnvAccess :: [Access],
    cnvAccessRole :: AccessRole,
    cnvName :: Maybe Text,
    cnvMembers :: ConvMembers,
    cnvTeam :: Maybe TeamId,
    cnvMessageTimer :: Maybe Milliseconds,
    cnvReceiptMode :: Maybe ReceiptMode
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Conversation)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Conversation

instance ToSchema Conversation where
  schema =
    objectWithDocModifier
      "Conversation"
      (description ?~ "A conversation object as returned from the server")
      $ Conversation
        <$> cnvId .= field "id" schema
        <*> cnvType .= field "type" schema
        <*> cnvCreator
          .= fieldWithDocModifier
            "creator"
            (description ?~ "The creator's user ID")
            schema
        <*> cnvAccess .= field "access" (array schema)
        <*> cnvAccessRole .= field "access_role" schema
        <*> cnvName .= lax (field "name" (optWithDefault A.Null schema))
        <*> cnvMembers .= field "members" schema
        <* const ("0.0" :: Text) .= optional (field "last_event" schema)
        <* const ("1970-01-01T00:00:00.000Z" :: Text)
          .= optional (field "last_event_time" schema)
        <*> cnvTeam .= lax (field "team" (optWithDefault A.Null schema))
        <*> cnvMessageTimer
          .= lax
            ( fieldWithDocModifier
                "message_timer"
                (description ?~ "Per-conversation message timer (can be null)")
                (optWithDefault A.Null schema)
            )
        <*> cnvReceiptMode .= lax (field "receipt_mode" (optWithDefault A.Null schema))

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

data ConversationList a = ConversationList
  { convList :: [a],
    convHasMore :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform (ConversationList a))

class ConversationListItem a where
  convListItemName :: Proxy a -> Text

instance ConversationListItem ConvId where
  convListItemName _ = "conversation IDs"

instance ConversationListItem Conversation where
  convListItemName _ = "conversations"

instance (ConversationListItem a, S.ToSchema a) => S.ToSchema (ConversationList a) where
  declareNamedSchema _ = do
    listSchema <- S.declareSchemaRef (Proxy @[a])
    pure $
      S.NamedSchema (Just "ConversationList") $
        mempty
          & description ?~ "Object holding a list of " <> convListItemName (Proxy @a)
          & S.properties . at "conversations" ?~ listSchema
          & S.properties . at "has_more"
            ?~ S.Inline
              ( S.toSchema (Proxy @Bool)
                  & description ?~ "Indicator that the server has more conversations than returned"
              )

instance ToJSON a => ToJSON (ConversationList a) where
  toJSON (ConversationList l m) =
    A.object
      [ "conversations" A..= l,
        "has_more" A..= m
      ]

instance FromJSON a => FromJSON (ConversationList a) where
  parseJSON = A.withObject "conversation-list" $ \o ->
    ConversationList
      <$> o A..: "conversations"
      <*> o A..: "has_more"

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
  deriving stock (Eq, Ord, Bounded, Enum, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Access)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema Access

instance ToSchema Access where
  schema =
    (S.schema . description ?~ "How users can join conversations") $
      enum @Text "Access" $
        mconcat
          [ element "private" PrivateAccess,
            element "invite" InviteAccess,
            element "link" LinkAccess,
            element "code" CodeAccess
          ]

typeAccess :: Doc.DataType
typeAccess = Doc.string . Doc.enum $ cs . A.encode <$> [(minBound :: Access) ..]

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
  deriving stock (Eq, Ord, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AccessRole)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema AccessRole

instance ToSchema AccessRole where
  schema =
    (S.schema . description ?~ "Which users can join conversations") $
      enum @Text "Access" $
        mconcat
          [ element "private" PrivateAccessRole,
            element "team" TeamAccessRole,
            element "activated" ActivatedAccessRole,
            element "non_activated" NonActivatedAccessRole
          ]

data ConvType
  = RegularConv
  | SelfConv
  | One2OneConv
  | ConnectConv
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConvType)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConvType

instance ToSchema ConvType where
  schema =
    enum @Integer "ConvType" $
      asum
        [ element 0 RegularConv,
          element 1 SelfConv,
          element 2 One2OneConv,
          element 3 ConnectConv
        ]

typeConversationType :: Doc.DataType
typeConversationType = Doc.int32 $ Doc.enum [0, 1, 2, 3]

-- | Define whether receipts should be sent in the given conversation
--   This datatype is defined as an int32 but the Backend does not
--   interpret it in any way, rather just stores and forwards it
--   for clients
--   E.g. of an implementation: 0 - send no ReceiptModes
--                              1 - send read ReceiptModes
--                              2 - send delivery ReceiptModes
--                              ...
newtype ReceiptMode = ReceiptMode {unReceiptMode :: Int32}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ReceiptMode

instance ToSchema ReceiptMode where
  schema =
    (S.schema . description ?~ "Conversation receipt mode") $
      ReceiptMode <$> unReceiptMode .= schema

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
  deriving stock (Eq, Show)

instance ToJSON NewConvManaged where
  toJSON (NewConvManaged nc) = newConvToJSON nc

instance FromJSON NewConvManaged where
  parseJSON v = do
    nc <- newConvParseJSON v
    unless (newConvIsManaged nc) $
      fail "only managed conversations are allowed here"
    pure (NewConvManaged nc)

instance Arbitrary NewConvManaged where
  arbitrary =
    NewConvManaged <$> (arbitrary `QC.suchThat` newConvIsManaged)

newtype NewConvUnmanaged = NewConvUnmanaged NewConv
  deriving stock (Eq, Show)
  deriving newtype (S.ToSchema)

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

instance S.ToSchema NewConv where
  declareNamedSchema _ =
    pure $
      S.NamedSchema (Just "NewConversation") $
        mempty
          & description ?~ "JSON object to create a new conversation"
          & S.properties . at "users"
            ?~ S.Inline
              ( S.toSchema (Proxy @[UserId])
                  & description ?~ "List of user IDs (excluding the requestor) to be part of this conversation"
              )
          & S.properties . at "name"
            ?~ S.Inline
              ( S.toSchema (Proxy @(Maybe Text))
                  & description ?~ "The conversation name"
              )
          & S.properties . at "team"
            ?~ S.Inline
              ( S.toSchema (Proxy @(Maybe ConvTeamInfo))
                  & description ?~ "Team information of this conversation"
              )
          & S.properties . at "access"
            ?~ S.Inline
              (S.toSchema (Proxy @(Set Access)))
          & S.properties . at "access_role"
            ?~ S.Inline
              (S.toSchema (Proxy @(Maybe AccessRole)))
          & S.properties . at "message_timer"
            ?~ S.Inline
              ( S.toSchema (Proxy @(Maybe Milliseconds))
                  & S.minimum_ ?~ 0
                  & description ?~ "Per-conversation message timer"
              )
          & S.properties . at "receipt_mode"
            ?~ S.Inline
              (S.toSchema (Proxy @(Maybe ReceiptMode)))
          & S.properties . at "conversation_role"
            ?~ S.Inline
              (S.toSchema (Proxy @RoleName))

instance ToJSON NewConvUnmanaged where
  toJSON (NewConvUnmanaged nc) = newConvToJSON nc

instance FromJSON NewConvUnmanaged where
  parseJSON v = do
    nc <- newConvParseJSON v
    when (newConvIsManaged nc) $
      fail "managed conversations have been deprecated"
    pure (NewConvUnmanaged nc)

instance Arbitrary NewConvUnmanaged where
  arbitrary =
    NewConvUnmanaged <$> (arbitrary `QC.suchThat` (not . newConvIsManaged))

data NewConv = NewConv
  { newConvUsers :: [UserId],
    newConvName :: Maybe Text,
    newConvAccess :: Set Access,
    newConvAccessRole :: Maybe AccessRole,
    newConvTeam :: Maybe ConvTeamInfo,
    newConvMessageTimer :: Maybe Milliseconds,
    newConvReceiptMode :: Maybe ReceiptMode,
    -- | Every member except for the creator will have this role
    newConvUsersRole :: RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewConv)

newConvIsManaged :: NewConv -> Bool
newConvIsManaged = maybe False cnvManaged . newConvTeam

newConvParseJSON :: Value -> A.Parser NewConv
newConvParseJSON = A.withObject "new-conv object" $ \i ->
  NewConv
    <$> i A..: "users"
    <*> i A..:? "name"
    <*> i A..:? "access" A..!= mempty
    <*> i A..:? "access_role"
    <*> i A..:? "team"
    <*> i A..:? "message_timer"
    <*> i A..:? "receipt_mode"
    <*> i A..:? "conversation_role" A..!= roleNameWireAdmin

newConvToJSON :: NewConv -> Value
newConvToJSON i =
  A.object $
    "users" A..= newConvUsers i
      # "name" A..= newConvName i
      # "access" A..= newConvAccess i
      # "access_role" A..= newConvAccessRole i
      # "team" A..= newConvTeam i
      # "message_timer" A..= newConvMessageTimer i
      # "receipt_mode" A..= newConvReceiptMode i
      # "conversation_role" A..= newConvUsersRole i
      # []

data ConvTeamInfo = ConvTeamInfo
  { cnvTeamId :: TeamId,
    cnvManaged :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConvTeamInfo)

instance S.ToSchema ConvTeamInfo where
  declareNamedSchema _ =
    pure $
      S.NamedSchema (Just "TeamInfo") $
        mempty
          & description ?~ "Team information"
          & S.properties . at "teamid"
            ?~ S.Inline
              ( S.toSchema (Proxy @TeamId)
                  & description ?~ "Team ID"
              )
          & S.properties . at "managed"
            ?~ S.Inline
              ( S.toSchema (Proxy @Bool)
                  & description ?~ "Whether this is a managed team conversation"
              )

modelTeamInfo :: Doc.Model
modelTeamInfo = Doc.defineModel "TeamInfo" $ do
  Doc.description "Team information"
  Doc.property "teamid" Doc.bytes' $
    Doc.description "Team ID"
  Doc.property "managed" Doc.bool' $
    Doc.description "Is this a managed team conversation?"

instance ToJSON ConvTeamInfo where
  toJSON c =
    A.object
      [ "teamid" A..= cnvTeamId c,
        "managed" A..= cnvManaged c
      ]

instance FromJSON ConvTeamInfo where
  parseJSON = A.withObject "conversation team info" $ \o ->
    ConvTeamInfo <$> o A..: "teamid" <*> o A..:? "managed" A..!= False

--------------------------------------------------------------------------------
-- invite

data Invite = Invite
  { invUsers :: List1 UserId,
    -- | This role name is to be applied to all users
    invRoleName :: RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Invite)

newInvite :: List1 UserId -> Invite
newInvite us = Invite us roleNameWireAdmin

modelInvite :: Doc.Model
modelInvite = Doc.defineModel "Invite" $ do
  Doc.description "Add users to a conversation"
  Doc.property "users" (Doc.unique $ Doc.array Doc.bytes') $
    Doc.description "List of user IDs to add to a conversation"

instance ToJSON Invite where
  toJSON i =
    A.object
      [ "users" A..= invUsers i,
        "conversation_role" A..= invRoleName i
      ]

instance FromJSON Invite where
  parseJSON = A.withObject "invite object" $ \o ->
    Invite <$> o A..: "users" <*> o A..:? "conversation_role" A..!= roleNameWireAdmin

--------------------------------------------------------------------------------
-- update

newtype ConversationRename = ConversationRename
  { cupName :: Text
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (ToJSON, FromJSON) via Schema ConversationRename

instance ToSchema ConversationRename where
  schema =
    object "ConversationRename" $
      ConversationRename
        <$> cupName
          .= fieldWithDocModifier
            "name"
            (description ?~ desc)
            (unnamed (schema @Text))
    where
      desc = "The new conversation name"

modelConversationUpdateName :: Doc.Model
modelConversationUpdateName = Doc.defineModel "ConversationUpdateName" $ do
  Doc.description "Contains conversation name to update"
  Doc.property "name" Doc.string' $
    Doc.description "The new conversation name"

data ConversationAccessUpdate = ConversationAccessUpdate
  { cupAccess :: [Access],
    cupAccessRole :: AccessRole
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationAccessUpdate)

modelConversationAccessUpdate :: Doc.Model
modelConversationAccessUpdate = Doc.defineModel "ConversationAccessUpdate" $ do
  Doc.description "Contains conversation properties to update"
  Doc.property "access" (Doc.unique $ Doc.array typeAccess) $
    Doc.description "List of conversation access modes."
  Doc.property "access_role" (Doc.bytes') $
    Doc.description "Conversation access role: private|team|activated|non_activated"

instance ToJSON ConversationAccessUpdate where
  toJSON c =
    A.object $
      "access" A..= cupAccess c
        # "access_role" A..= cupAccessRole c
        # []

instance FromJSON ConversationAccessUpdate where
  parseJSON = A.withObject "conversation-access-update" $ \o ->
    ConversationAccessUpdate
      <$> o A..: "access"
      <*> o A..: "access_role"

data ConversationReceiptModeUpdate = ConversationReceiptModeUpdate
  { cruReceiptMode :: ReceiptMode
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationReceiptModeUpdate)
  deriving (ToJSON, FromJSON) via Schema ConversationReceiptModeUpdate

instance ToSchema ConversationReceiptModeUpdate where
  schema =
    objectWithDocModifier "ConversationReceiptModeUpdate" (description ?~ desc) $
      ConversationReceiptModeUpdate
        <$> cruReceiptMode .= field "receipt_mode" (unnamed schema)
    where
      desc =
        "Contains conversation receipt mode to update to. Receipt mode tells \
        \clients whether certain types of receipts should be sent in the given \
        \conversation or not. How this value is interpreted is up to clients."

modelConversationReceiptModeUpdate :: Doc.Model
modelConversationReceiptModeUpdate = Doc.defineModel "conversationReceiptModeUpdate" $ do
  Doc.description
    "Contains conversation receipt mode to update to. Receipt mode tells \
    \clients whether certain types of receipts should be sent in the given \
    \conversation or not. How this value is interpreted is up to clients."
  Doc.property "receipt_mode" Doc.int32' $
    Doc.description "Receipt mode: int32"

data ConversationMessageTimerUpdate = ConversationMessageTimerUpdate
  { -- | New message timer
    cupMessageTimer :: Maybe Milliseconds
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationMessageTimerUpdate)

modelConversationMessageTimerUpdate :: Doc.Model
modelConversationMessageTimerUpdate = Doc.defineModel "ConversationMessageTimerUpdate" $ do
  Doc.description "Contains conversation properties to update"
  Doc.property "message_timer" Doc.int64' $
    Doc.description "Conversation message timer (in milliseconds); can be null"

instance ToJSON ConversationMessageTimerUpdate where
  toJSON c =
    A.object
      [ "message_timer" A..= cupMessageTimer c
      ]

instance FromJSON ConversationMessageTimerUpdate where
  parseJSON = A.withObject "conversation-message-timer-update" $ \o ->
    ConversationMessageTimerUpdate <$> o A..:? "message_timer"
