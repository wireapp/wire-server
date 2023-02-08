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

    -- * List of MLS client ids
    ClientList (..),

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

    -- * create
    NewConv (..),
    ConvTeamInfo (..),

    -- * invite
    Invite (..),
    InviteQualified (..),
    newInvite,

    -- * update
    ConversationRename (..),
    ConversationAccessData (..),
    conversationAccessDataSchema,
    ConversationReceiptModeUpdate (..),
    ConversationMessageTimerUpdate (..),
    ConversationJoin (..),
    ConversationMemberUpdate (..),

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
    modelConversationAccessData,
    modelConversationReceiptModeUpdate,
    modelConversationMessageTimerUpdate,
    typeConversationType,
    typeAccess,
  )
where

import Control.Applicative
import Control.Lens ((?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Id
import Data.List.Extra (disjointOrd)
import Data.List.NonEmpty (NonEmpty)
import Data.List1
import Data.Misc
import Data.Qualified (Qualified (qUnqualified), deprecatedSchema)
import Data.Range (Range, fromRange, rangedSchema)
import Data.SOP
import Data.Schema
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUIDV5
import Imports
import Servant.API
import System.Random (randomRIO)
import Wire.API.Conversation.Member
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role (RoleName, roleNameWireAdmin)
import Wire.API.MLS.Group
import Wire.API.Routes.MultiTablePaging
import Wire.API.Routes.MultiVerb
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.Arbitrary

--------------------------------------------------------------------------------
-- Conversation

data ConversationMetadata = ConversationMetadata
  { cnvmType :: ConvType,
    -- FUTUREWORK: Make this a qualified user ID.
    cnvmCreator :: UserId,
    cnvmAccess :: [Access],
    cnvmAccessRoles :: Set AccessRole,
    cnvmName :: Maybe Text,
    -- FUTUREWORK: Think if it makes sense to make the team ID qualified due to
    -- federation.
    cnvmTeam :: Maybe TeamId,
    cnvmMessageTimer :: Maybe Milliseconds,
    cnvmReceiptMode :: Maybe ReceiptMode
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationMetadata)
  deriving (FromJSON, ToJSON) via Schema ConversationMetadata

defConversationMetadata :: UserId -> ConversationMetadata
defConversationMetadata creator =
  ConversationMetadata
    { cnvmType = RegularConv,
      cnvmCreator = creator,
      cnvmAccess = [PrivateAccess],
      cnvmAccessRoles = mempty,
      cnvmName = Nothing,
      cnvmTeam = Nothing,
      cnvmMessageTimer = Nothing,
      cnvmReceiptMode = Nothing
    }

accessRolesVersionedSchema :: Version -> ObjectSchema SwaggerDoc (Set AccessRole)
accessRolesVersionedSchema v =
  if v > V2 then accessRolesSchema else accessRolesSchemaV2

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
      .= fieldWithDocModifier
        "creator"
        (description ?~ "The creator's user ID")
        schema
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

instance ToSchema ConversationMetadata where
  schema = object "ConversationMetadata" (conversationMetadataObjectSchema accessRolesSchema)

instance ToSchema (Versioned 'V2 ConversationMetadata) where
  schema =
    Versioned
      <$> unVersioned
        .= object
          "ConversationMetadata"
          (conversationMetadataObjectSchema accessRolesSchemaV2)

-- | Public-facing client list
data ClientList = ClientList {clClients :: [ClientId]}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ClientList)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ClientList

instance ToSchema ClientList where
  schema =
    object "ClientList" $
      ClientList
        <$> clClients
          .= field "client_ids" (array schema)

-- | Public-facing conversation type. Represents information that a
-- particular user is allowed to see.
--
-- Can be produced from the internal one ('Galley.Data.Types.Conversation')
-- by using 'Galley.API.Mapping.conversationView'.
data Conversation = Conversation
  { -- | A qualified conversation ID
    cnvQualifiedId :: Qualified ConvId,
    cnvMetadata :: ConversationMetadata,
    cnvMembers :: ConvMembers,
    -- | The protocol of the conversation. It can be Proteus or MLS (1.0).
    cnvProtocol :: Protocol
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Conversation)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Conversation

cnvType :: Conversation -> ConvType
cnvType = cnvmType . cnvMetadata

cnvCreator :: Conversation -> UserId
cnvCreator = cnvmCreator . cnvMetadata

cnvAccess :: Conversation -> [Access]
cnvAccess = cnvmAccess . cnvMetadata

cnvAccessRoles :: Conversation -> Set AccessRole
cnvAccessRoles = cnvmAccessRoles . cnvMetadata

cnvName :: Conversation -> Maybe Text
cnvName = cnvmName . cnvMetadata

cnvTeam :: Conversation -> Maybe TeamId
cnvTeam = cnvmTeam . cnvMetadata

cnvMessageTimer :: Conversation -> Maybe Milliseconds
cnvMessageTimer = cnvmMessageTimer . cnvMetadata

cnvReceiptMode :: Conversation -> Maybe ReceiptMode
cnvReceiptMode = cnvmReceiptMode . cnvMetadata

instance ToSchema Conversation where
  schema = conversationSchema V3

instance ToSchema (Versioned 'V2 Conversation) where
  schema = Versioned <$> unVersioned .= conversationSchema V2

conversationSchema ::
  Version ->
  ValueSchema NamedSwaggerDoc Conversation
conversationSchema v =
  objectWithDocModifier
    "Conversation"
    (description ?~ "A conversation object as returned from the server")
    $ Conversation
      <$> cnvQualifiedId .= field "qualified_id" schema
      <* (qUnqualified . cnvQualifiedId)
        .= optional (field "id" (deprecatedSchema "qualified_id" schema))
      <*> cnvMetadata .= conversationMetadataObjectSchema (accessRolesVersionedSchema v)
      <*> cnvMembers .= field "members" schema
      <*> cnvProtocol .= protocolSchema

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

-- | Limited view of a 'Conversation'. Is used to inform users with an invite
-- link about the conversation.
data ConversationCoverView = ConversationCoverView
  { cnvCoverConvId :: ConvId,
    cnvCoverName :: Maybe Text
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

instance ConversationListItem Conversation where
  convListItemName _ = "conversations"

instance (ConversationListItem a, ToSchema a) => ToSchema (ConversationList a) where
  schema = conversationListSchema schema

instance ToSchema (Versioned 'V2 (ConversationList Conversation)) where
  schema =
    Versioned
      <$> unVersioned
        .= conversationListSchema (conversationSchema V2)

conversationListSchema ::
  forall a.
  ConversationListItem a =>
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
  { crFound :: [Conversation],
    crNotFound :: [Qualified ConvId],
    crFailed :: [Qualified ConvId]
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationsResponse

conversationsResponseSchema ::
  Version ->
  ValueSchema NamedSwaggerDoc ConversationsResponse
conversationsResponseSchema v =
  let notFoundDoc = description ?~ "These conversations either don't exist or are deleted."
      failedDoc = description ?~ "The server failed to fetch these conversations, most likely due to network issues while contacting a remote server"
   in objectWithDocModifier
        "ConversationsResponse"
        (description ?~ "Response object for getting metadata of a list of conversations")
        $ ConversationsResponse
          <$> crFound .= field "found" (array (conversationSchema v))
          <*> crNotFound .= fieldWithDocModifier "not_found" notFoundDoc (array schema)
          <*> crFailed .= fieldWithDocModifier "failed" failedDoc (array schema)

instance ToSchema ConversationsResponse where
  schema = conversationsResponseSchema V3

instance ToSchema (Versioned 'V2 ConversationsResponse) where
  schema = Versioned <$> unVersioned .= conversationsResponseSchema V2

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

genEnumSet :: forall a. (Bounded a, Enum a, Ord a, Eq a, Show a) => [a] -> [a] -> IO (Either String (Set a))
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
    allMember :: Ord a => Set a -> Set a -> Bool
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
  deriving stock (Eq, Show, Generic)
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

-- | Used to describe a 'NewConv'.
modelNewConversation :: Doc.Model
modelNewConversation = Doc.defineModel "NewConversation" $ do
  Doc.description "JSON object to create a new conversation"
  Doc.property "users" (Doc.unique $ Doc.array Doc.bytes') $
    Doc.description "List of user IDs (excluding the requestor) to be part of this conversation"
  Doc.property "qualified_users" (Doc.unique . Doc.array $ Doc.bytes') $
    Doc.description "List of qualified user IDs to be part of this conversation"
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
    newConvProtocol :: ProtocolTag,
    -- | ID of the client creating the conversation. Only needed for MLS
    -- conversations.
    newConvCreatorClient :: Maybe ClientId
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform NewConv)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema NewConv)

instance ToSchema NewConv where
  schema =
    newConvSchema $
      maybe_ (optField "access_role" (set schema))

instance ToSchema (Versioned 'V2 NewConv) where
  schema = Versioned <$> unVersioned .= newConvSchema accessRolesSchemaOptV2

newConvSchema ::
  ObjectSchema SwaggerDoc (Maybe (Set AccessRole)) ->
  ValueSchema NamedSwaggerDoc NewConv
newConvSchema sch =
  objectWithDocModifier
    "NewConv"
    (description ?~ "JSON object to create a new conversation. When using 'qualified_users' (preferred), you can omit 'users'")
    $ NewConv
      <$> newConvUsers
        .= ( fieldWithDocModifier
               "users"
               (description ?~ usersDesc)
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
      <*> newConvProtocol .= protocolTagSchema
      <*> newConvCreatorClient .= maybe_ (optField "creator_client" schema)
  where
    usersDesc =
      "List of user IDs (excluding the requestor) to be \
      \part of this conversation (deprecated)"
    qualifiedUsersDesc =
      "List of qualified user IDs (excluding the requestor) \
      \to be part of this conversation"
    usersRoleDesc :: Text
    usersRoleDesc =
      cs $
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
      c :: ToJSON a => a -> ValueSchema SwaggerDoc ()
      c val = mkSchema mempty (const (pure ())) (const (pure (toJSON val)))

modelTeamInfo :: Doc.Model
modelTeamInfo = Doc.defineModel "TeamInfo" $ do
  Doc.description "Team information"
  Doc.property "teamid" Doc.bytes' $
    Doc.description "Team ID"
  Doc.property "managed" Doc.bool' $
    Doc.description managedDesc

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
  { invQUsers :: NonEmpty (Qualified UserId),
    -- | This role name is to be applied to all users
    invQRoleName :: RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform InviteQualified)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema InviteQualified)

instance ToSchema InviteQualified where
  schema =
    object "InviteQualified" $
      InviteQualified
        <$> invQUsers .= field "qualified_users" (nonEmptyArray schema)
        <*> invQRoleName
          .= (fromMaybe roleNameWireAdmin <$> optField "conversation_role" schema)

newInvite :: List1 UserId -> Invite
newInvite us = Invite us roleNameWireAdmin

modelInvite :: Doc.Model
modelInvite = Doc.defineModel "Invite" $ do
  Doc.description "Add users to a conversation"
  Doc.property "users" (Doc.unique $ Doc.array Doc.bytes') $
    Doc.description "List of user IDs to add to a conversation"

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

modelConversationUpdateName :: Doc.Model
modelConversationUpdateName = Doc.defineModel "ConversationUpdateName" $ do
  Doc.description "Contains conversation name to update"
  Doc.property "name" Doc.string' $
    Doc.description "The new conversation name"

data ConversationAccessData = ConversationAccessData
  { cupAccess :: Set Access,
    cupAccessRoles :: Set AccessRole
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConversationAccessData)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationAccessData

conversationAccessDataSchema :: Version -> ValueSchema NamedSwaggerDoc ConversationAccessData
conversationAccessDataSchema v =
  object ("ConversationAccessData" <> suffix) $
    ConversationAccessData
      <$> cupAccess .= field "access" (set schema)
      <*> cupAccessRoles .= accessRolesVersionedSchema v
  where
    suffix
      | v == maxBound = ""
      | otherwise = toUrlPiece v

instance ToSchema ConversationAccessData where
  schema = conversationAccessDataSchema V3

instance ToSchema (Versioned 'V2 ConversationAccessData) where
  schema = Versioned <$> unVersioned .= conversationAccessDataSchema V2

modelConversationAccessData :: Doc.Model
modelConversationAccessData = Doc.defineModel "ConversationAccessData" $ do
  Doc.description "Contains conversation properties to update"
  Doc.property "access" (Doc.unique $ Doc.array typeAccess) $
    Doc.description "List of conversation access modes."
  Doc.property "access_role" Doc.bytes' $
    Doc.description "Conversation access role: private|team|activated|non_activated"

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
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationMessageTimerUpdate

instance ToSchema ConversationMessageTimerUpdate where
  schema =
    objectWithDocModifier
      "ConversationMessageTimerUpdate"
      (description ?~ "Contains conversation properties to update")
      $ ConversationMessageTimerUpdate
        <$> cupMessageTimer .= optField "message_timer" (maybeWithDefault A.Null schema)

modelConversationMessageTimerUpdate :: Doc.Model
modelConversationMessageTimerUpdate = Doc.defineModel "ConversationMessageTimerUpdate" $ do
  Doc.description "Contains conversation properties to update"
  Doc.property "message_timer" Doc.int64' $
    Doc.description "Conversation message timer (in milliseconds); can be null"

data ConversationJoin = ConversationJoin
  { cjUsers :: NonEmpty (Qualified UserId),
    cjRole :: RoleName
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
        <$> cjUsers .= field "users" (nonEmptyArray schema)
        <*> cjRole .= field "role" schema

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

-- | The id of the MLS self conversation for a given user
mlsSelfConvId :: UserId -> ConvId
mlsSelfConvId uid =
  let inputBytes = LBS.unpack . UUID.toByteString . toUUID $ uid
   in Id (UUIDV5.generateNamed namespaceMLSSelfConv inputBytes)

namespaceMLSSelfConv :: UUID.UUID
namespaceMLSSelfConv =
  -- a V5 uuid created with the nil namespace
  fromJust . UUID.fromString $ "3eac2a2c-3850-510b-bd08-8a98e80dd4d9"

--------------------------------------------------------------------------------
-- MultiVerb instances

instance AsHeaders '[ConvId] Conversation Conversation where
  toHeaders c = (I (qUnqualified (cnvQualifiedId c)) :* Nil, c)
  fromHeaders = snd
