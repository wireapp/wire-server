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

-- FUTUREWORK: There's still a lot of stuff we should factor out into separate
-- modules.
module Wire.API.Conversation
  ( -- * Conversation
    ConversationMetadata (..),
    defConversationMetadata,
    ConversationV8 (..),
    Conversation (..),
    conversationSchema,
    cnvType,
    cnvCreator,
    cnvAccess,
    cnvName,
    cnvTeam,
    cnvMessageTimer,
    cnvReceiptMode,
    cnvAccessRoles,
    MLSOne2OneConversation (..),
    CreateGroupConversationV8 (..),
    CreateGroupConversation (..),
    ConversationCoverView (..),
    ConversationList (..),
    ListConversations (..),
    GetPaginatedConversationIds,
    pattern GetPaginatedConversationIds,
    ConvIdsPage,
    pattern ConvIdsPage,
    ConversationPagingState,
    pattern ConversationPagingState,
    ConversationsResponse (..),
    GroupId (..),
    mlsSelfConvId,

    -- * Conversation properties
    Access (..),
    AccessRole (..),
    accessRolesSchemaV2,
    genAccessRolesV2,
    AccessRoleLegacy (..),
    ConvType (..),
    ReceiptMode (..),
    fromAccessRoleLegacy,
    toAccessRoleLegacy,
    defRole,
    maybeRole,
    AddPermission (..),

    -- * create
    NewConv (..),
    GroupConvType (..),
    NewOne2OneConv (..),
    ConvTeamInfo (..),

    -- * invite
    Invite (..),
    InviteQualified (..),

    -- * update
    ConversationRename (..),
    ConversationAccessData (..),
    conversationAccessDataSchema,
    ConversationReceiptModeUpdate (..),
    ConversationMessageTimerUpdate (..),
    JoinType (..),
    ConversationJoin (..),
    ConversationMemberUpdate (..),
    ConversationRemoveMembers (..),
    AddPermissionUpdate (..),

    -- * delete
    ConversationDelete (..),

    -- * re-exports
    module Wire.API.Conversation.Member,
  )
where

import Control.Applicative
import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.ByteString.Lazy qualified as LBS
import Data.Default
import Data.Domain
import Data.Id
import Data.List.Extra (disjointOrd)
import Data.List.NonEmpty (NonEmpty)
import Data.List1
import Data.Map qualified as Map
import Data.Misc
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Range (Range, fromRange, rangedSchema)
import Data.SOP
import Data.Schema
import Data.Set qualified as Set
import Data.Singletons
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V5 qualified as UUIDV5
import Imports
import System.Random (randomRIO)
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Member
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role (RoleName, roleNameWireAdmin)
import Wire.API.Event.LeaveReason
import Wire.API.MLS.Group
import Wire.API.MLS.Keys
import Wire.API.Routes.MultiTablePaging
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.API.User
import Wire.Arbitrary

--------------------------------------------------------------------------------
-- Conversation

data ConversationMetadata = ConversationMetadata
  { cnvmType :: ConvType,
    -- FUTUREWORK: Make this a qualified user ID.
    cnvmCreator :: Maybe UserId,
    cnvmAccess :: [Access],
    cnvmAccessRoles :: Set AccessRole,
    cnvmName :: Maybe Text,
    -- FUTUREWORK: Think if it makes sense to make the team ID qualified due to
    -- federation.
    cnvmTeam :: Maybe TeamId,
    cnvmMessageTimer :: Maybe Milliseconds,
    cnvmReceiptMode :: Maybe ReceiptMode,
    cnvmGroupConvType :: Maybe GroupConvType,
    cnvmChannelAddPermission :: Maybe AddPermission,
    cnvmCellsState :: CellsState
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationMetadata)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationMetadata

defConversationMetadata :: Maybe UserId -> ConversationMetadata
defConversationMetadata mCreator =
  ConversationMetadata
    { cnvmType = RegularConv,
      cnvmCreator = mCreator,
      cnvmAccess = [PrivateAccess],
      cnvmAccessRoles = mempty,
      cnvmName = Nothing,
      cnvmTeam = Nothing,
      cnvmMessageTimer = Nothing,
      cnvmReceiptMode = Nothing,
      cnvmGroupConvType = Just GroupConversation,
      cnvmChannelAddPermission = Nothing,
      cnvmCellsState = def
    }

accessRolesVersionedSchema :: Maybe Version -> ObjectSchema SwaggerDoc (Set AccessRole)
accessRolesVersionedSchema (Just v) | v < V3 = accessRolesSchemaV2
accessRolesVersionedSchema _ = accessRolesSchema

accessRolesSchema :: ObjectSchema SwaggerDoc (Set AccessRole)
accessRolesSchema = field "access_role" (set schema)

accessRolesSchemaV2 :: ObjectSchema SwaggerDoc (Set AccessRole)
accessRolesSchemaV2 = toOutput .= accessRolesSchemaTuple `withParser` validate
  where
    toOutput accessRoles = (Just $ toAccessRoleLegacy accessRoles, Just accessRoles)
    validate =
      \case
        (_, Just v2) -> pure v2
        (Just legacy, Nothing) -> pure $ fromAccessRoleLegacy legacy
        (Nothing, Nothing) -> fail "access_role|access_role_v2"

accessRolesSchemaOptV2 :: ObjectSchema SwaggerDoc (Maybe (Set AccessRole))
accessRolesSchemaOptV2 = toOutput .= accessRolesSchemaTuple `withParser` validate
  where
    toOutput accessRoles = (toAccessRoleLegacy <$> accessRoles, accessRoles)
    validate =
      \case
        (_, Just v2) -> pure $ Just v2
        (Just legacy, Nothing) -> pure $ Just (fromAccessRoleLegacy legacy)
        (Nothing, Nothing) -> pure Nothing

accessRolesSchemaTuple :: ObjectSchema SwaggerDoc (Maybe AccessRoleLegacy, Maybe (Set AccessRole))
accessRolesSchemaTuple =
  (,)
    <$> fst .= optFieldWithDocModifier "access_role" (description ?~ "Deprecated, please use access_role_v2") (maybeWithDefault A.Null schema)
    <*> snd .= optField "access_role_v2" (maybeWithDefault A.Null $ set schema)

conversationMetadataObjectSchema ::
  ObjectSchema SwaggerDoc (Set AccessRole) ->
  ObjectSchema SwaggerDoc ConversationMetadata
conversationMetadataObjectSchema sch =
  ConversationMetadata
    <$> cnvmType .= field "type" schema
    <*> cnvmCreator
      .= optFieldWithDocModifier
        "creator"
        (description ?~ "The creator's user ID")
        (maybeWithDefault A.Null schema)
    <*> cnvmAccess .= field "access" (array schema)
    <*> cnvmAccessRoles .= sch
    <*> cnvmName .= optField "name" (maybeWithDefault A.Null schema)
    <* const ("0.0" :: Text) .= optional (field "last_event" schema)
    <* const ("1970-01-01T00:00:00.000Z" :: Text)
      .= optional (field "last_event_time" schema)
    <*> cnvmTeam .= optField "team" (maybeWithDefault A.Null schema)
    <*> cnvmMessageTimer
      .= optFieldWithDocModifier
        "message_timer"
        (description ?~ "Per-conversation message timer (can be null)")
        (maybeWithDefault A.Null schema)
    <*> cnvmReceiptMode .= optField "receipt_mode" (maybeWithDefault A.Null schema)
    <*> cnvmGroupConvType .= optField "group_conv_type" (maybeWithDefault A.Null schema)
    <*> cnvmChannelAddPermission .= optField "add_permission" (maybeWithDefault A.Null schema)
    <*> cnvmCellsState .= (fromMaybe def <$> optField "cells_state" schema)

instance ToSchema ConversationMetadata where
  schema = object "ConversationMetadata" (conversationMetadataObjectSchema accessRolesSchema)

instance ToSchema (Versioned 'V2 ConversationMetadata) where
  schema =
    Versioned
      <$> unVersioned
        .= object
          "ConversationMetadata"
          (conversationMetadataObjectSchema accessRolesSchemaV2)

-- | Public-facing conversation type. Represents information that a
-- particular user is allowed to see.
--
-- Can be produced from the internal one ('Galley.Data.Types.Conversation')
-- by using 'Galley.API.Mapping.conversationView'.
data ConversationV8 = ConversationV8
  { -- | A qualified conversation ID
    cnvQualifiedId :: Qualified ConvId,
    cnvMetadata :: ConversationMetadata,
    cnvMembers :: ConvMembers,
    -- | The protocol of the conversation. It can be Proteus or MLS (1.0).
    cnvProtocol :: Protocol
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationV8)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationV8

cnvType :: ConversationV8 -> ConvType
cnvType = cnvmType . cnvMetadata

cnvCreator :: ConversationV8 -> Maybe UserId
cnvCreator = cnvmCreator . cnvMetadata

cnvAccess :: ConversationV8 -> [Access]
cnvAccess = cnvmAccess . cnvMetadata

cnvAccessRoles :: ConversationV8 -> Set AccessRole
cnvAccessRoles = cnvmAccessRoles . cnvMetadata

cnvName :: ConversationV8 -> Maybe Text
cnvName = cnvmName . cnvMetadata

cnvTeam :: ConversationV8 -> Maybe TeamId
cnvTeam = cnvmTeam . cnvMetadata

cnvMessageTimer :: ConversationV8 -> Maybe Milliseconds
cnvMessageTimer = cnvmMessageTimer . cnvMetadata

cnvReceiptMode :: ConversationV8 -> Maybe ReceiptMode
cnvReceiptMode = cnvmReceiptMode . cnvMetadata

instance ToSchema ConversationV8 where
  schema = conversationSchema Nothing

instance (SingI v) => ToSchema (Versioned v ConversationV8) where
  schema = Versioned <$> unVersioned .= conversationSchema (Just (demote @v))

conversationObjectSchema :: Maybe Version -> ObjectSchema SwaggerDoc ConversationV8
conversationObjectSchema v =
  ConversationV8
    <$> cnvQualifiedId .= field "qualified_id" schema
    <* (qUnqualified . cnvQualifiedId)
      .= optional (field "id" (deprecatedSchema "qualified_id" schema))
    <*> cnvMetadata .= conversationMetadataObjectSchema (accessRolesVersionedSchema v)
    <*> cnvMembers .= field "members" schema
    <*> cnvProtocol .= protocolSchema v

conversationSchema ::
  Maybe Version ->
  ValueSchema NamedSwaggerDoc ConversationV8
conversationSchema v =
  objectWithDocModifier
    ("Conversation" <> foldMap (Text.toUpper . versionText) v)
    (description ?~ "A conversation object as returned from the server")
    (conversationObjectSchema v)

data Conversation = Conversation
  { qualifiedId :: Qualified ConvId,
    metadata :: ConversationMetadata,
    members :: Set OtherMember,
    protocol :: Protocol
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Conversation)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Conversation

instance ToSchema Conversation where
  schema =
    objectWithDocModifier
      "Conversation"
      (description ?~ "A conversation object as returned from the server")
      $ conversationV9ObjectSchema

conversationV9ObjectSchema :: ObjectSchema SwaggerDoc Conversation
conversationV9ObjectSchema =
  Conversation
    <$> qualifiedId .= field "qualified_id" schema
    <*> metadata .= conversationMetadataObjectSchema accessRolesSchema
    <*> members .= field "members" (set schema)
    <*> protocol .= protocolSchema Nothing

data MLSOne2OneConversation a = MLSOne2OneConversation
  { conversation :: ConversationV8,
    publicKeys :: MLSKeysByPurpose (MLSKeys a)
  }
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (MLSOne2OneConversation a))

instance (ToSchema a) => ToSchema (MLSOne2OneConversation a) where
  schema =
    let aName = maybe "" ("_" <>) $ getName (schemaDoc (schema @a))
     in object ("MLSOne2OneConversation" <> aName) $
          MLSOne2OneConversation
            <$> (.conversation) .= field "conversation" schema
            <*> publicKeys .= field "public_keys" schema

-- | The public-facing conversation type extended with information on which
-- remote users could not be added when creating the conversation.
data CreateGroupConversationV8 = CreateGroupConversationV8
  { cgcConversation :: ConversationV8,
    -- | Remote users that could not be added to the created group conversation
    -- because their backend was not reachable.
    cgcFailedToAdd :: Map Domain (Set UserId)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CreateGroupConversationV8)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema CreateGroupConversationV8

instance ToSchema CreateGroupConversationV8 where
  schema = createGroupConversationSchema Nothing

instance (SingI v) => ToSchema (Versioned v CreateGroupConversationV8) where
  schema = Versioned <$> unVersioned .= createGroupConversationSchema (Just (demote @v))

createGroupConversationSchema :: Maybe Version -> ValueSchema NamedSwaggerDoc CreateGroupConversationV8
createGroupConversationSchema v =
  objectWithDocModifier
    "CreateGroupConversationV8"
    (description ?~ "A created group-conversation object extended with a list of failed-to-add users")
    $ CreateGroupConversationV8
      <$> cgcConversation .= conversationObjectSchema v
      <*> (toFlatList . cgcFailedToAdd)
        .= field "failed_to_add" (fromFlatList <$> array schema)

toFlatList :: Map Domain (Set a) -> [Qualified a]
toFlatList m =
  (\(d, s) -> flip Qualified d <$> Set.toList s) =<< Map.assocs m

fromFlatList :: (Ord a) => [Qualified a] -> Map Domain (Set a)
fromFlatList = fmap Set.fromList . indexQualified

data CreateGroupConversation = CreateGroupConversation
  { conversation :: Conversation,
    failedToAdd :: Map Domain (Set UserId)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CreateGroupConversation)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema CreateGroupConversation

instance ToSchema CreateGroupConversation where
  schema =
    objectWithDocModifier
      "CreateGroupConversation"
      (description ?~ "A created group-conversation object extended with a list of failed-to-add users")
      $ CreateGroupConversation
        <$> (.conversation) .= conversationV9ObjectSchema
        <*> (toFlatList . failedToAdd) .= field "failed_to_add" (fromFlatList <$> array schema)

-- | Limited view of a 'Conversation'. Is used to inform users with an invite
-- link about the conversation.
data ConversationCoverView = ConversationCoverView
  { cnvCoverConvId :: ConvId,
    cnvCoverName :: Maybe Text,
    cnvCoverHasPassword :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationCoverView)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationCoverView

-- | Schema is compatible to a subset of 'Conversation' schema, in case we
-- decide to substitute 'ConversationCoverView' with it in the future.
instance ToSchema ConversationCoverView where
  schema =
    objectWithDocModifier
      "ConversationCoverView"
      (description ?~ "Limited view of Conversation.")
      $ ConversationCoverView
        <$> cnvCoverConvId .= field "id" schema
        <*> cnvCoverName .= optField "name" (maybeWithDefault A.Null schema)
        <*> cnvCoverHasPassword .= field "has_password" schema

data ConversationList a = ConversationList
  { convList :: [a],
    convHasMore :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform (ConversationList a))
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema (ConversationList a)

class ConversationListItem a where
  convListItemName :: Proxy a -> Text

instance ConversationListItem ConvId where
  convListItemName _ = "conversation IDs"

instance ConversationListItem ConversationV8 where
  convListItemName _ = "conversations"

instance (ConversationListItem a, ToSchema a) => ToSchema (ConversationList a) where
  schema = conversationListSchema schema

instance ToSchema (Versioned 'V2 (ConversationList ConversationV8)) where
  schema =
    Versioned
      <$> unVersioned
        .= conversationListSchema (conversationSchema (Just V2))

conversationListSchema ::
  forall a.
  (ConversationListItem a) =>
  ValueSchema NamedSwaggerDoc a ->
  ValueSchema NamedSwaggerDoc (ConversationList a)
conversationListSchema sch =
  objectWithDocModifier
    "ConversationList"
    (description ?~ "Object holding a list of " <> convListItemName (Proxy @a))
    $ ConversationList
      <$> convList .= field "conversations" (array sch)
      <*> convHasMore
        .= fieldWithDocModifier
          "has_more"
          (description ?~ "Indicator that the server has more conversations than returned")
          schema

type ConversationPagingName = "ConversationIds"

type ConvIdPagingKey = "qualified_conversations"

type ConversationPagingState = MultiTablePagingState ConversationPagingName LocalOrRemoteTable

pattern ConversationPagingState :: tables -> Maybe ByteString -> MultiTablePagingState name tables
pattern ConversationPagingState table state = MultiTablePagingState table state

type ConvIdsPage = MultiTablePage ConversationPagingName ConvIdPagingKey LocalOrRemoteTable (Qualified ConvId)

pattern ConvIdsPage :: [a] -> Bool -> MultiTablePagingState name tables -> MultiTablePage name resultsKey tables a
pattern ConvIdsPage ids hasMore state = MultiTablePage ids hasMore state

type GetPaginatedConversationIds = GetMultiTablePageRequest ConversationPagingName LocalOrRemoteTable 1000 1000

pattern GetPaginatedConversationIds :: Maybe (MultiTablePagingState name tables) -> Range 1 max Int32 -> GetMultiTablePageRequest name tables max def
pattern GetPaginatedConversationIds state size = GetMultiTablePageRequest size state

-- | Used on the POST /conversations/list/v2 endpoint
newtype ListConversations = ListConversations
  { lcQualifiedIds :: Range 1 1000 [Qualified ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ListConversations

instance ToSchema ListConversations where
  schema =
    objectWithDocModifier
      "ListConversations"
      (description ?~ "A request to list some of a user's conversations, including remote ones. Maximum 1000 qualified conversation IDs")
      $ ListConversations
        <$> (fromRange . lcQualifiedIds) .= field "qualified_ids" (rangedSchema (array schema))

data ConversationsResponse = ConversationsResponse
  { crFound :: [ConversationV8],
    crNotFound :: [Qualified ConvId],
    crFailed :: [Qualified ConvId]
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationsResponse

conversationsResponseSchema ::
  Maybe Version ->
  ValueSchema NamedSwaggerDoc ConversationsResponse
conversationsResponseSchema v =
  let notFoundDoc = description ?~ "These conversations either don't exist or are deleted."
      failedDoc = description ?~ "The server failed to fetch these conversations, most likely due to network issues while contacting a remote server"
   in objectWithDocModifier
        ("ConversationsResponse" <> foldMap (Text.toUpper . versionText) v)
        (description ?~ "Response object for getting metadata of a list of conversations")
        $ ConversationsResponse
          <$> crFound .= field "found" (array (conversationSchema v))
          <*> crNotFound .= fieldWithDocModifier "not_found" notFoundDoc (array schema)
          <*> crFailed .= fieldWithDocModifier "failed" failedDoc (array schema)

instance ToSchema ConversationsResponse where
  schema = conversationsResponseSchema Nothing

instance (SingI v) => ToSchema (Versioned v ConversationsResponse) where
  schema = Versioned <$> unVersioned .= conversationsResponseSchema (Just (demote @v))

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

-- | AccessRoles define who can join conversations. The roles are
-- "supersets", i.e. Activated includes Team and NonActivated includes
-- Activated.
data AccessRoleLegacy
  = -- | Nobody can be invited to this conversation
    --   (e.g. it's a 1:1 conversation)
    PrivateAccessRole
  | -- | Team-only conversation
    TeamAccessRole
  | -- | Conversation for users who have activated
    --   email, phone or SSO and bots
    ActivatedAccessRole
  | -- | No checks
    NonActivatedAccessRole
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)
  deriving (Arbitrary) via (GenericUniform AccessRoleLegacy)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema AccessRoleLegacy

fromAccessRoleLegacy :: AccessRoleLegacy -> Set AccessRole
fromAccessRoleLegacy = \case
  PrivateAccessRole -> privateAccessRole
  TeamAccessRole -> teamAccessRole
  ActivatedAccessRole -> activatedAccessRole
  NonActivatedAccessRole -> nonActivatedAccessRole

privateAccessRole :: Set AccessRole
privateAccessRole = Set.fromList []

teamAccessRole :: Set AccessRole
teamAccessRole = Set.fromList [TeamMemberAccessRole]

activatedAccessRole :: Set AccessRole
activatedAccessRole = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, ServiceAccessRole]

nonActivatedAccessRole :: Set AccessRole
nonActivatedAccessRole = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole, ServiceAccessRole]

defRole :: Set AccessRole
defRole = activatedAccessRole

maybeRole :: ConvType -> Maybe (Set AccessRole) -> Set AccessRole
maybeRole SelfConv _ = privateAccessRole
maybeRole ConnectConv _ = privateAccessRole
maybeRole One2OneConv _ = privateAccessRole
maybeRole RegularConv Nothing = defRole
maybeRole RegularConv (Just r) = r

data AccessRole
  = TeamMemberAccessRole
  | NonTeamMemberAccessRole
  | GuestAccessRole
  | ServiceAccessRole
  deriving stock (Eq, Ord, Show, Generic, Bounded, Enum)
  deriving (Arbitrary) via (GenericUniform AccessRole)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema AccessRole

genAccessRolesV2 :: [AccessRole] -> [AccessRole] -> IO (Either String (Set AccessRole))
genAccessRolesV2 = genEnumSet

genEnumSet :: forall a. (Bounded a, Enum a, Ord a, Show a) => [a] -> [a] -> IO (Either String (Set a))
genEnumSet with without =
  if disjointOrd with without
    then do
      let xs = Set.toList . Set.powerSet . Set.fromList $ [minBound ..]
      x <- (xs !!) <$> randomRIO (0, length xs - 1)
      pure . Right . Set.fromList $ (Set.toList x <> with) \\ without
    else do
      pure $ Left ("overlapping arguments: " <> show (with, without))

toAccessRoleLegacy :: Set AccessRole -> AccessRoleLegacy
toAccessRoleLegacy accessRoles = do
  fromMaybe NonActivatedAccessRole $ find (allMember accessRoles . fromAccessRoleLegacy) [minBound ..]
  where
    allMember :: (Ord a) => Set a -> Set a -> Bool
    allMember rhs lhs = all (`Set.member` lhs) rhs

instance ToSchema AccessRole where
  schema =
    (S.schema . description ?~ desc) $
      enum @Text "AccessRole" $
        mconcat
          [ element "team_member" TeamMemberAccessRole,
            element "non_team_member" NonTeamMemberAccessRole,
            element "guest" GuestAccessRole,
            element "service" ServiceAccessRole
          ]
    where
      desc =
        "Which users/services can join conversations.\
        \ This replaces legacy access roles and allows a more fine grained\
        \ configuration of access roles, and in particular a separation of\
        \ guest and services access.\n\nThis field is optional. If it is not\
        \ present, the default will be `[team_member, non_team_member, service]`.\
        \ Please note that an empty list is not allowed when creating a new\
        \ conversation."

instance ToSchema AccessRoleLegacy where
  schema =
    (S.schema . S.deprecated ?~ True) $
      (S.schema . description ?~ desc) $
        enum @Text "AccessRoleLegacy" $
          mconcat
            [ element "private" PrivateAccessRole,
              element "team" TeamAccessRole,
              element "activated" ActivatedAccessRole,
              element "non_activated" NonActivatedAccessRole
            ]
    where
      desc =
        "Which users can join conversations (deprecated, use `access_role_v2` instead).\
        \Maps to `access_role_v2` as follows:\
        \`private` => `[]` - nobody can be invited to this conversation (e.g. it's a 1:1 conversation)\
        \`team` => `[team_member]` - team-only conversation\
        \`activated` => `[team_member, non_team_member, service]` - conversation for users who have activated email, phone or SSO and services\
        \`non_activated` => `[team_member, non_team_member, service, guest]` - all allowed, no checks\
        \\
        \Maps from `access_role_v2` as follows:\
        \`[]` => `private` - nobody can be invited to this conversation (e.g. it's a 1:1 conversation)\
        \`[team_member]` => `team` - team-only conversation\
        \`[team_member, non_team_member, service]` => `activated` - conversation for users who have activated email, phone or SSO and services\
        \`[team_member, non_team_member, service, guest]` => `non_activated` - all allowed, no checks.\
        \All other configurations of `access_role_v2` are mapped to the smallest superset containing all given access roles."

data ConvType
  = RegularConv
  | SelfConv
  | One2OneConv
  | ConnectConv
  deriving stock (Eq, Show, Enum, Generic)
  deriving (Arbitrary) via (GenericUniform ConvType)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConvType

instance ToSchema ConvType where
  schema =
    enum @Integer "ConvType" $
      mconcat
        [ element 0 RegularConv,
          element 1 SelfConv,
          element 2 One2OneConv,
          element 3 ConnectConv
        ]

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

instance Default ReceiptMode where
  def = ReceiptMode 0

instance ToSchema ReceiptMode where
  schema =
    (S.schema . description ?~ "Conversation receipt mode") $
      ReceiptMode <$> unReceiptMode .= schema

--------------------------------------------------------------------------------
-- create

data GroupConvType = GroupConversation | Channel
  deriving stock (Eq, Show, Generic, Enum)
  deriving (Arbitrary) via (GenericUniform GroupConvType)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema GroupConvType

instance ToSchema GroupConvType where
  schema =
    enum @Text "GroupConvType" $
      mconcat
        [ element "group_conversation" GroupConversation,
          element "channel" Channel
        ]

data NewConv = NewConv
  { newConvUsers :: [UserId],
    -- | A list of qualified users, which can include some local qualified users
    -- too.
    newConvQualifiedUsers :: [Qualified UserId],
    newConvName :: Maybe (Range 1 256 Text),
    newConvAccess :: Set Access,
    newConvAccessRoles :: Maybe (Set AccessRole),
    newConvTeam :: Maybe ConvTeamInfo,
    newConvMessageTimer :: Maybe Milliseconds,
    newConvReceiptMode :: Maybe ReceiptMode,
    -- | Every member except for the creator will have this role
    newConvUsersRole :: RoleName,
    -- | The protocol of the conversation. It can be Proteus or MLS (1.0).
    newConvProtocol :: BaseProtocolTag,
    newConvGroupConvType :: GroupConvType,
    newConvCells :: Bool,
    newConvChannelAddPermission :: Maybe AddPermission,
    newConvSkipCreator :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewConv)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema NewConv)

instance ToSchema NewConv where
  schema =
    newConvSchema Nothing $
      maybe_ (optField "access_role" (set schema))

instance ToSchema (Versioned 'V2 NewConv) where
  schema = Versioned <$> unVersioned .= newConvSchema (Just V2) accessRolesSchemaOptV2

newConvSchema ::
  Maybe Version ->
  ObjectSchema SwaggerDoc (Maybe (Set AccessRole)) ->
  ValueSchema NamedSwaggerDoc NewConv
newConvSchema v sch =
  objectWithDocModifier
    ("NewConv" <> foldMap (Text.toUpper . versionText) v)
    (description ?~ "JSON object to create a new conversation. When using 'qualified_users' (preferred), you can omit 'users'")
    $ NewConv
      <$> newConvUsers
        .= ( fieldWithDocModifier
               "users"
               ( (S.deprecated ?~ True)
                   . (description ?~ usersDesc)
               )
               (array schema)
               <|> pure []
           )
      <*> newConvQualifiedUsers
        .= ( fieldWithDocModifier
               "qualified_users"
               (description ?~ qualifiedUsersDesc)
               (array schema)
               <|> pure []
           )
      <*> newConvName .= maybe_ (optField "name" schema)
      <*> (Set.toList . newConvAccess)
        .= (fromMaybe mempty <$> optField "access" (Set.fromList <$> array schema))
      <*> newConvAccessRoles .= sch
      <*> newConvTeam
        .= maybe_
          ( optFieldWithDocModifier
              "team"
              (description ?~ "Team information of this conversation")
              schema
          )
      <*> newConvMessageTimer
        .= maybe_
          ( optFieldWithDocModifier
              "message_timer"
              (description ?~ "Per-conversation message timer")
              schema
          )
      <*> newConvReceiptMode .= maybe_ (optField "receipt_mode" schema)
      <*> newConvUsersRole
        .= ( fieldWithDocModifier "conversation_role" (description ?~ usersRoleDesc) schema
               <|> pure roleNameWireAdmin
           )
      <*> newConvProtocol
        .= fmap
          (fromMaybe BaseProtocolProteusTag)
          (optField "protocol" schema)
      <*> newConvGroupConvType .= (fromMaybe GroupConversation <$> optField "group_conv_type" schema)
      <*> newConvCells .= (fromMaybe False <$> optField "cells" schema)
      <*> newConvChannelAddPermission
        .= maybe_
          ( optFieldWithDocModifier "add_permission" (description ?~ "Channel add permission") schema
          )
      <*> newConvSkipCreator
        .= ( fromMaybe False
               <$> optFieldWithDocModifier
                 "skip_creator"
                 (description ?~ "Don't add creator to the conversation, only works for team admins not wanting to be part of the channels they create.")
                 schema
           )
  where
    usersDesc =
      "List of user IDs (excluding the requestor) to be \
      \part of this conversation (deprecated)"
    qualifiedUsersDesc =
      "List of qualified user IDs (excluding the requestor) \
      \to be part of this conversation"
    usersRoleDesc :: Text
    usersRoleDesc =
      Text.pack $
        "The conversation permissions the users \
        \added in this request should have. \
        \Optional, defaults to '"
          <> show roleNameWireAdmin
          <> "' if unset."

newtype ConvTeamInfo = ConvTeamInfo
  { cnvTeamId :: TeamId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConvTeamInfo)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema ConvTeamInfo)

managedDesc :: Text
managedDesc =
  "This field MUST NOT be used by clients. "
    <> "It is here only for backwards compatibility of the interface."

instance ToSchema ConvTeamInfo where
  schema =
    objectWithDocModifier
      "ConvTeamInfo"
      (description ?~ "Team information")
      $ ConvTeamInfo
        <$> cnvTeamId .= field "teamid" schema
        <* const ()
          .= fieldWithDocModifier
            "managed"
            (description ?~ managedDesc)
            (c (False :: Bool))
    where
      c :: (ToJSON a) => a -> ValueSchema SwaggerDoc ()
      c val = mkSchema mempty (const (pure ())) (const (pure (toJSON val)))

data NewOne2OneConv = NewOne2OneConv
  { users :: [UserId],
    -- | A list of qualified users, which can include some local qualified users
    -- too.
    qualifiedUsers :: [Qualified UserId],
    name :: Maybe (Range 1 256 Text),
    team :: Maybe ConvTeamInfo
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewOne2OneConv)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema NewOne2OneConv)

instance ToSchema NewOne2OneConv where
  schema =
    objectWithDocModifier
      "NewOne2OneConv"
      (description ?~ "JSON object to create a new 1:1 conversation. When using 'qualified_users' (preferred), you can omit 'users'")
      $ NewOne2OneConv
        <$> (.users)
          .= ( fieldWithDocModifier
                 "users"
                 ( (S.deprecated ?~ True)
                     . (description ?~ usersDesc)
                 )
                 (array schema)
                 <|> pure []
             )
        <*> (.qualifiedUsers)
          .= ( fieldWithDocModifier
                 "qualified_users"
                 (description ?~ qualifiedUsersDesc)
                 (array schema)
                 <|> pure []
             )
        <*> name .= maybe_ (optField "name" schema)
        <*> team
          .= maybe_
            ( optFieldWithDocModifier
                "team"
                (description ?~ "Team information of this conversation")
                schema
            )
    where
      usersDesc =
        "List of user IDs (excluding the requestor) to be \
        \part of this conversation (deprecated)"
      qualifiedUsersDesc =
        "List of qualified user IDs (excluding the requestor) \
        \to be part of this conversation"

--------------------------------------------------------------------------------
-- invite

data Invite = Invite -- Deprecated, use InviteQualified (and maybe rename?)
  { invUsers :: List1 UserId,
    -- | This role name is to be applied to all users
    invRoleName :: RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Invite)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema Invite)

instance ToSchema Invite where
  schema =
    object "Invite" $
      Invite
        <$> (toNonEmpty . invUsers)
          .= fmap List1 (field "users" (nonEmptyArray schema))
        <*> invRoleName
          .= (fromMaybe roleNameWireAdmin <$> optField "conversation_role" schema)

data InviteQualified = InviteQualified
  { users :: NonEmpty (Qualified UserId),
    -- | This role name is to be applied to all users
    roleName :: RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform InviteQualified)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema InviteQualified)

instance ToSchema InviteQualified where
  schema =
    object "InviteQualified" $
      InviteQualified
        <$> (.users) .= field "qualified_users" (nonEmptyArray schema)
        <*> roleName .= (fromMaybe roleNameWireAdmin <$> optField "conversation_role" schema)

--------------------------------------------------------------------------------
-- update

newtype ConversationRename = ConversationRename
  { cupName :: Text
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (S.ToSchema, ToJSON, FromJSON) via Schema ConversationRename

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

data ConversationAccessData = ConversationAccessData
  { cupAccess :: Set Access,
    cupAccessRoles :: Set AccessRole
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationAccessData)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationAccessData

conversationAccessDataSchema :: Maybe Version -> ValueSchema NamedSwaggerDoc ConversationAccessData
conversationAccessDataSchema v =
  object ("ConversationAccessData" <> foldMap (Text.toUpper . versionText) v) $
    ConversationAccessData
      <$> cupAccess .= field "access" (set schema)
      <*> cupAccessRoles .= accessRolesVersionedSchema v

instance ToSchema ConversationAccessData where
  schema = conversationAccessDataSchema Nothing

instance ToSchema (Versioned 'V2 ConversationAccessData) where
  schema = Versioned <$> unVersioned .= conversationAccessDataSchema (Just V2)

data ConversationReceiptModeUpdate = ConversationReceiptModeUpdate
  { cruReceiptMode :: ReceiptMode
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationReceiptModeUpdate)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema ConversationReceiptModeUpdate

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

data ConversationMessageTimerUpdate = ConversationMessageTimerUpdate
  { -- | New message timer
    cupMessageTimer :: Maybe Milliseconds
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationMessageTimerUpdate)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationMessageTimerUpdate

instance ToSchema ConversationMessageTimerUpdate where
  schema =
    objectWithDocModifier
      "ConversationMessageTimerUpdate"
      (description ?~ "Contains conversation properties to update")
      $ ConversationMessageTimerUpdate
        <$> cupMessageTimer .= optField "message_timer" (maybeWithDefault A.Null schema)

data JoinType = ExternalAdd | InternalAdd
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform JoinType)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema JoinType

instance Default JoinType where
  def = InternalAdd

instance ToSchema JoinType where
  schema =
    enum @Text "JoinType" $
      mconcat
        [ element "external_add" ExternalAdd,
          element "internal_add" InternalAdd
        ]

data ConversationJoin = ConversationJoin
  { users :: NonEmpty (Qualified UserId),
    role :: RoleName,
    joinType :: JoinType
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationJoin)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationJoin

instance ToSchema ConversationJoin where
  schema =
    objectWithDocModifier
      "ConversationJoin"
      (description ?~ "The action of some users joining a conversation")
      $ ConversationJoin
        <$> (.users) .= field "users" (nonEmptyArray schema)
        <*> role .= field "role" schema
        <*> joinType .= (fromMaybe def <$> optField "join_type" schema)

data ConversationMemberUpdate = ConversationMemberUpdate
  { cmuTarget :: Qualified UserId,
    cmuUpdate :: OtherMemberUpdate
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationMemberUpdate)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationMemberUpdate

instance ToSchema ConversationMemberUpdate where
  schema =
    objectWithDocModifier
      "ConversationMemberUpdate"
      (description ?~ "The action of promoting/demoting a member of a conversation")
      $ ConversationMemberUpdate
        <$> cmuTarget .= field "target" schema
        <*> cmuUpdate .= field "update" schema

data ConversationRemoveMembers = ConversationRemoveMembers
  { crmTargets :: NonEmpty (Qualified UserId),
    crmReason :: EdMemberLeftReason
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationRemoveMembers)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationRemoveMembers

instance ToSchema ConversationRemoveMembers where
  schema =
    objectWithDocModifier
      "ConversationRemoveMembers"
      (description ?~ "The action of removing members from a conversation")
      $ ConversationRemoveMembers
        <$> crmTargets .= field "targets" (nonEmptyArray schema)
        <*> crmReason .= field "reason" schema

-- | The id of the MLS self conversation for a given user
mlsSelfConvId :: UserId -> ConvId
mlsSelfConvId uid =
  let inputBytes = LBS.unpack . UUID.toByteString . toUUID $ uid
   in Id (UUIDV5.generateNamed namespaceMLSSelfConv inputBytes)

namespaceMLSSelfConv :: UUID.UUID
namespaceMLSSelfConv =
  -- a V5 uuid created with the nil namespace
  fromJust . UUID.fromString $ "3eac2a2c-3850-510b-bd08-8a98e80dd4d9"

data AddPermission = Admins | Everyone
  deriving stock (Eq, Show, Generic, Enum)
  deriving (Arbitrary) via (GenericUniform AddPermission)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema AddPermission

instance Default AddPermission where
  def = Everyone

instance ToSchema AddPermission where
  schema =
    enum @Text "AddPermission" $
      mconcat
        [ element "admins" Admins,
          element "everyone" Everyone
        ]

newtype AddPermissionUpdate = AddPermissionUpdate
  { addPermission :: AddPermission
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform AddPermissionUpdate)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema AddPermissionUpdate

instance ToSchema AddPermissionUpdate where
  schema =
    objectWithDocModifier
      "AddPermissionUpdate"
      (description ?~ "The action of changing the permission to add members to a channel")
      $ AddPermissionUpdate
        <$> addPermission .= field "add_permission" schema

newtype ConversationDelete = ConversationDelete
  { teamId :: TeamId
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary)
  deriving (S.ToSchema, ToJSON, FromJSON) via Schema ConversationDelete

instance ToSchema ConversationDelete where
  schema =
    objectWithDocModifier
      "ConversationDelete"
      (description ?~ "The action of deleting a conversation")
      $ ConversationDelete
        <$> teamId
          .= field
            "teamId"
            (unnamed (schema @TeamId))

--------------------------------------------------------------------------------
-- MultiVerb instances

instance AsHeaders '[ConvId] ConversationV8 ConversationV8 where
  toHeaders c = (I (qUnqualified (cnvQualifiedId c)) :* Nil, c)
  fromHeaders = snd

instance AsHeaders '[ConvId] Conversation Conversation where
  toHeaders c = (I (qUnqualified c.qualifiedId) :* Nil, c)
  fromHeaders = snd

instance AsHeaders '[ConvId] CreateGroupConversationV8 CreateGroupConversationV8 where
  toHeaders c =
    ((I . qUnqualified . cnvQualifiedId . cgcConversation $ c) :* Nil, c)
  fromHeaders = snd

instance AsHeaders '[ConvId] CreateGroupConversation CreateGroupConversation where
  toHeaders c =
    (I c.conversation.qualifiedId.qUnqualified :* Nil, c)
  fromHeaders = snd
