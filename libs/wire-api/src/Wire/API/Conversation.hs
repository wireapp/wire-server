{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
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

-- FUTUREWORK: There's still a lot of stuff we should factor out into separate
-- modules.
module Wire.API.Conversation
  ( -- * Conversation
    Conversation (..),
    ConversationCoverView (..),
    ConversationList (..),
    ListConversations (..),
    ListConversationsV2 (..),
    GetPaginatedConversationIds (..),
    ConversationPagingState (..),
    ConversationPagingTable (..),
    ConvIdsPage (..),
    ConversationsResponse (..),

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
    InviteQualified (..),
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
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString as BS
import Data.Id
import Data.Json.Util (fromBase64Text, toBase64Text)
import Data.List.NonEmpty (NonEmpty)
import Data.List1
import Data.Misc
import Data.Proxy (Proxy (Proxy))
import Data.Qualified (Qualified (qUnqualified), deprecatedSchema)
import Data.Range (Range, rangedSchema, toRange, fromRange)
import Data.Schema
import Data.Singletons (sing)
import qualified Data.Set as Set
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
  { -- | A qualified conversation ID
    cnvQualifiedId :: Qualified ConvId,
    cnvType :: ConvType,
    -- FUTUREWORK: Make this a qualified user ID.
    cnvCreator :: UserId,
    cnvAccess :: [Access],
    cnvAccessRole :: AccessRole,
    cnvName :: Maybe Text,
    cnvMembers :: ConvMembers,
    -- FUTUREWORK: Think if it makes sense to make the team ID qualified due to
    -- federation.
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
        <$> cnvQualifiedId .= field "qualified_id" schema
        <* (qUnqualified . cnvQualifiedId)
          .= optional (field "id" (deprecatedSchema "qualified_id" schema))
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
        <*> cnvCoverName .= lax (field "name" (optWithDefault A.Null schema))

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

instance ConversationListItem (Qualified ConvId) where
  convListItemName _ = "qualified Conversation IDs"

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

data ConvIdsPage = ConvIdsPage
  { pageConvIds :: [Qualified ConvId],
    pageHasMore :: Bool,
    pagePagingState :: ConversationPagingState
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConvIdsPage

instance ToSchema ConvIdsPage where
  schema =
    object "ConvIdsPage" $
      ConvIdsPage
        <$> pageConvIds .= field "qualified_conversations" (array schema)
        <*> pageHasMore .= field "has_more" schema
        <*> pagePagingState .= field "paging_state" schema

data ConversationPagingState = ConversationPagingState
  { cpsTable :: ConversationPagingTable,
    cpsPagingState :: Maybe ByteString
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationPagingState

instance ToSchema ConversationPagingState where
  schema =
    (toBase64Text . encodeConversationPagingState)
      .= parsedText "ConversationPagingState" (parseConvesationPagingState <=< fromBase64Text)

parseConvesationPagingState :: ByteString -> Either String ConversationPagingState
parseConvesationPagingState = AB.parseOnly conversationPagingStateParser

conversationPagingStateParser :: AB.Parser ConversationPagingState
conversationPagingStateParser = do
  cpsTable <- tableParser
  cpsPagingState <- (AB.endOfInput $> Nothing) <|> (Just <$> AB.takeByteString <* AB.endOfInput)
  pure ConversationPagingState {..}
  where
    tableParser :: AB.Parser ConversationPagingTable
    tableParser =
      (AB.word8 0 $> PagingLocals)
        <|> (AB.word8 1 $> PagingRemotes)

encodeConversationPagingState :: ConversationPagingState -> ByteString
encodeConversationPagingState ConversationPagingState {..} =
  let table = encodeConversationPagingTable cpsTable
      state = fromMaybe "" cpsPagingState
   in BS.cons table state

encodeConversationPagingTable :: ConversationPagingTable -> Word8
encodeConversationPagingTable = \case
  PagingLocals -> 0
  PagingRemotes -> 1

data ConversationPagingTable
  = PagingLocals
  | PagingRemotes
  deriving (Show, Eq)

data GetPaginatedConversationIds = GetPaginatedConversationIds
  { gpciPagingState :: Maybe ConversationPagingState,
    gpciSize :: Range 1 1000 Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema GetPaginatedConversationIds

instance ToSchema GetPaginatedConversationIds where
  schema =
    let addPagingStateDoc =
          description
            ?~ "optional, when not specified first page of the conversation ids will be returned.\
               \Every returned page contains a paging_state, this should be supplied to retrieve the next page."
        addSizeDoc = description ?~ "optional, must be <= 1000, defaults to 1000."
     in objectWithDocModifier
          "GetPaginatedConversationIds"
          (description ?~ "A request to list some or all of a user's conversation ids, including remote ones")
          $ GetPaginatedConversationIds
            <$> gpciPagingState .= optFieldWithDocModifier "paging_state" Nothing addPagingStateDoc schema
            <*> gpciSize .= (fieldWithDocModifier "size" addSizeDoc schema <|> pure (toRange (Proxy @1000)))

data ListConversations = ListConversations
  { lQualifiedIds :: Maybe (NonEmpty (Qualified ConvId)),
    lStartId :: Maybe (Qualified ConvId),
    lSize :: Maybe (Range 1 500 Int32)
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ListConversations

instance ToSchema ListConversations where
  schema =
    objectWithDocModifier
      "ListConversations"
      (description ?~ "A request to list some or all of a user's conversations, including remote ones")
      $ ListConversations
        <$> lQualifiedIds .= optField "qualified_ids" Nothing (nonEmptyArray schema)
        <*> lStartId .= optField "start_id" Nothing schema
        <*> lSize .= optField "size" Nothing schema

-- | Used on the POST /list-conversations endpoint
newtype ListConversationsV2 = ListConversationsV2
  { lcQualifiedIds :: Range 1 1000 [Qualified ConvId]
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ListConversationsV2

instance ToSchema ListConversationsV2 where
  schema =
    objectWithDocModifier
      "ListConversations"
      (description ?~ "A request to list some of a user's conversations, including remote ones")
      $ ListConversationsV2
        <$> (fromRange . lcQualifiedIds) .= field "qualified_ids" (rangedSchema sing sing (array schema))

data ConversationsResponse = ConversationsResponse
  { crFound :: [Conversation],
    crNotFound :: [Qualified ConvId],
    crFailed :: [Qualified ConvId]
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationsResponse

instance ToSchema ConversationsResponse where
  schema =
    let notFoundDoc = description ?~ "These conversations either don't exist or are deleted."
        failedDoc = description ?~ "The server failed to fetch these conversations, most likely due to network issues while contacting a remote server"
     in objectWithDocModifier
          "ConversationsResponse"
          (description ?~ "Response object for getting metadata of a list of conversations")
          $ ConversationsResponse
            <$> crFound .= field "found" (array schema)
            <*> crNotFound .= fieldWithDocModifier "not_found" notFoundDoc (array schema)
            <*> crFailed .= fieldWithDocModifier "failed" failedDoc (array schema)

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
  deriving (FromJSON, ToJSON) via Schema NewConvManaged

instance ToSchema NewConvManaged where
  schema = NewConvManaged <$> unwrap .= newConvSchema `withParser` check
    where
      unwrap (NewConvManaged c) = c
      check c
        | newConvIsManaged c = pure c
        | otherwise = fail "only managed conversations are allowed here"

instance Arbitrary NewConvManaged where
  arbitrary =
    NewConvManaged <$> (arbitrary `QC.suchThat` newConvIsManaged)

newtype NewConvUnmanaged = NewConvUnmanaged NewConv
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema NewConvUnmanaged

-- | Used to describe a 'NewConvUnmanaged'.
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

instance ToSchema NewConvUnmanaged where
  schema = NewConvUnmanaged <$> unwrap .= newConvSchema `withParser` check
    where
      unwrap (NewConvUnmanaged c) = c
      check c
        | newConvIsManaged c =
          fail "managed conversations have been deprecated"
        | otherwise = pure c

instance Arbitrary NewConvUnmanaged where
  arbitrary =
    NewConvUnmanaged <$> (arbitrary `QC.suchThat` (not . newConvIsManaged))

data NewConv = NewConv
  { newConvUsers :: [UserId],
    -- | A list of qualified users, which can include some local qualified users
    -- too.
    newConvQualifiedUsers :: [Qualified UserId],
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

newConvSchema :: ValueSchema NamedSwaggerDoc NewConv
newConvSchema =
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
      <*> newConvName .= opt (field "name" schema)
      <*> (Set.toList . newConvAccess)
        .= ( field "access" (Set.fromList <$> array schema)
               <|> pure mempty
           )
      <*> newConvAccessRole .= opt (field "access_role" schema)
      <*> newConvTeam
        .= opt
          ( fieldWithDocModifier
              "team"
              (description ?~ "Team information of this conversation")
              schema
          )
      <*> newConvMessageTimer
        .= opt
          ( fieldWithDocModifier
              "message_timer"
              (description ?~ "Per-conversation message timer")
              schema
          )
      <*> newConvReceiptMode .= opt (field "receipt_mode" schema)
      <*> newConvUsersRole
        .= ( fieldWithDocModifier "conversation_role" (description ?~ usersRoleDesc) schema
               <|> pure roleNameWireAdmin
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
      cs $
        "The conversation permissions the users \
        \added in this request should have. \
        \Optional, defaults to '"
          <> show roleNameWireAdmin
          <> "' if unset."

newConvIsManaged :: NewConv -> Bool
newConvIsManaged = maybe False cnvManaged . newConvTeam

data ConvTeamInfo = ConvTeamInfo
  { cnvTeamId :: TeamId,
    cnvManaged :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConvTeamInfo)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConvTeamInfo

instance ToSchema ConvTeamInfo where
  schema =
    objectWithDocModifier
      "ConvTeamInfo"
      (description ?~ "Team information")
      $ ConvTeamInfo
        <$> cnvTeamId .= field "teamid" schema
        <*> cnvManaged
          .= ( fieldWithDocModifier
                 "managed"
                 (description ?~ "Whether this is a managed team conversation")
                 schema
                 <|> pure False
             )

modelTeamInfo :: Doc.Model
modelTeamInfo = Doc.defineModel "TeamInfo" $ do
  Doc.description "Team information"
  Doc.property "teamid" Doc.bytes' $
    Doc.description "Team ID"
  Doc.property "managed" Doc.bool' $
    Doc.description "Is this a managed team conversation?"

--------------------------------------------------------------------------------
-- invite

data Invite = Invite -- Deprecated, use InviteQualified (and maybe rename?)
  { invUsers :: List1 UserId,
    -- | This role name is to be applied to all users
    invRoleName :: RoleName
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform Invite)

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
        <*> invQRoleName .= (field "conversation_role" schema <|> pure roleNameWireAdmin)

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
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema ConversationAccessUpdate

instance ToSchema ConversationAccessUpdate where
  schema =
    object "ConversationAccessUpdate" $
      ConversationAccessUpdate
        <$> cupAccess .= field "access" (array schema)
        <*> cupAccessRole .= field "access_role" schema

modelConversationAccessUpdate :: Doc.Model
modelConversationAccessUpdate = Doc.defineModel "ConversationAccessUpdate" $ do
  Doc.description "Contains conversation properties to update"
  Doc.property "access" (Doc.unique $ Doc.array typeAccess) $
    Doc.description "List of conversation access modes."
  Doc.property "access_role" (Doc.bytes') $
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
        <$> cupMessageTimer .= lax (field "message_timer" (optWithDefault A.Null schema))

modelConversationMessageTimerUpdate :: Doc.Model
modelConversationMessageTimerUpdate = Doc.defineModel "ConversationMessageTimerUpdate" $ do
  Doc.description "Contains conversation properties to update"
  Doc.property "message_timer" Doc.int64' $
    Doc.description "Conversation message timer (in milliseconds); can be null"
