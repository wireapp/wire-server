{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Routes.Public.Galley where

import Control.Lens (at, over, (.~), (?~))
import Control.Lens.Combinators (_Just)
import qualified Data.Aeson as A
import Data.CommaSeparatedList
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Id (ConvId, TeamId, UserId)
import Data.Range
import Data.Schema
import qualified Data.Set as Set
import Data.Swagger (PathItem (..), Swagger (..))
import qualified Data.Swagger as Swagger
import qualified Data.Text as Text
import GHC.TypeLits (KnownNat, KnownSymbol, Symbol, natVal, symbolVal)
import GHC.TypeNats (Nat)
import Imports hiding (head)
import Servant hiding (Handler, JSON, addHeader, contentType, respond)
import qualified Servant
import Servant.API.Generic (ToServantApi, (:-))
import Servant.API.Status (KnownStatus)
import Servant.Swagger.Internal
import Servant.Swagger.Internal.Orphans ()
import qualified Wire.API.Conversation as Public
import qualified Wire.API.Conversation.Role as Public
import qualified Wire.API.Event.Conversation as Public
import qualified Wire.API.Event.Team as Public ()
import qualified Wire.API.Message as Public
import Wire.API.Routes.Public (EmptyResult, ZConn, ZUser)
import qualified Wire.API.Team.Conversation as Public

type ConversationResponses =
  '[ WithStatus 200 (Headers '[Servant.Header "Location" ConvId] Public.Conversation),
     WithStatus 201 (Headers '[Servant.Header "Location" ConvId] Public.Conversation)
   ]

type UpdateResponses =
  '[ WithStatus 200 Public.Event,
     NoContent
   ]

data ErrorDescription (status :: Nat) (desc :: Symbol) = ErrorDescription
  { label :: !Text,
    message :: !Text
  }
  deriving stock (Show, Typeable)
  deriving (A.ToJSON, A.FromJSON, Swagger.ToSchema) via Schema (ErrorDescription status desc)

instance (KnownNat status, KnownSymbol desc) => ToSchema (ErrorDescription status desc) where
  schema =
    addDoc $
      object "ErrorDescription" $
        ErrorDescription
          <$> label .= field "label" schema
          <*> message .= field "message" schema
          <* const (natVal (Proxy @status)) .= field "status" genericToSchema
    where
      -- FUTUREWORK: Make this description go into swagger's response
      -- description
      addDoc sch =
        sch
          & Swagger.schema . Swagger.description ?~ Text.pack (symbolVal (Proxy @desc))

-- | This insance works with 'UVerb' only becaue of the following overlapping
-- instance for 'UVerb method cs (ErrorDescription status desc ': rest))'
instance (KnownNat status, KnownSymbol desc, AllAccept cs, SwaggerMethod method) => HasSwagger (Verb method status cs (ErrorDescription status desc)) where
  toSwagger _ = overrrideResponseDesc $ mkEndpoint "/" (Proxy @(Verb method status cs (Headers '[] (ErrorDescription status desc))))
    where
      overrrideResponseDesc :: Swagger -> Swagger
      overrrideResponseDesc =
        over (Swagger.paths . at "/" . _Just) overridePathItem
      overridePathItem :: Swagger.PathItem -> Swagger.PathItem
      overridePathItem =
        over (Swagger.get . _Just) overrideOp
          . over (Swagger.post . _Just) overrideOp
          . over (Swagger.put . _Just) overrideOp
          . over (Swagger.head_ . _Just) overrideOp
          . over (Swagger.patch . _Just) overrideOp
          . over (Swagger.delete . _Just) overrideOp
          . over (Swagger.options . _Just) overrideOp
      overrideOp :: Swagger.Operation -> Swagger.Operation
      overrideOp =
        Swagger.responses . Swagger.responses . at (fromInteger $ natVal (Proxy @status))
          ?~ Swagger.Inline
            ( mempty
                & Swagger.description .~ Text.pack (symbolVal (Proxy @desc))
                & Swagger.schema ?~ Swagger.toSchemaRef (Proxy @(ErrorDescription status desc))
            )

-- | This is a copy of instance for 'UVerb method cs (a:as)', but without this
-- things don't work because the instance defined in the library is already
-- compiled with the now overlapped version of `Verb method cs a` and won't
-- pickup the above instance.
instance
  (KnownNat status, KnownSymbol desc, AllAccept cs, SwaggerMethod method, HasSwagger (UVerb method cs rest)) =>
  HasSwagger (UVerb method cs (ErrorDescription status desc ': rest))
  where
  toSwagger _ =
    toSwagger (Proxy @(Verb method (StatusOf (ErrorDescription status desc)) cs (ErrorDescription status desc)))
      `combineSwagger` toSwagger (Proxy @(UVerb method cs rest))
    where
      -- workaround for https://github.com/GetShopTV/swagger2/issues/218
      -- We'd like to juse use (<>) but the instances are wrong
      combinePathItem :: PathItem -> PathItem -> PathItem
      combinePathItem s t =
        PathItem
          { _pathItemGet = _pathItemGet s <> _pathItemGet t,
            _pathItemPut = _pathItemPut s <> _pathItemPut t,
            _pathItemPost = _pathItemPost s <> _pathItemPost t,
            _pathItemDelete = _pathItemDelete s <> _pathItemDelete t,
            _pathItemOptions = _pathItemOptions s <> _pathItemOptions t,
            _pathItemHead = _pathItemHead s <> _pathItemHead t,
            _pathItemPatch = _pathItemPatch s <> _pathItemPatch t,
            _pathItemParameters = _pathItemParameters s <> _pathItemParameters t
          }

      combineSwagger :: Swagger -> Swagger -> Swagger
      combineSwagger s t =
        Swagger
          { _swaggerInfo = _swaggerInfo s <> _swaggerInfo t,
            _swaggerHost = _swaggerHost s <|> _swaggerHost t,
            _swaggerBasePath = _swaggerBasePath s <|> _swaggerBasePath t,
            _swaggerSchemes = _swaggerSchemes s <> _swaggerSchemes t,
            _swaggerConsumes = _swaggerConsumes s <> _swaggerConsumes t,
            _swaggerProduces = _swaggerProduces s <> _swaggerProduces t,
            _swaggerPaths = InsOrdHashMap.unionWith combinePathItem (_swaggerPaths s) (_swaggerPaths t),
            _swaggerDefinitions = _swaggerDefinitions s <> _swaggerDefinitions t,
            _swaggerParameters = _swaggerParameters s <> _swaggerParameters t,
            _swaggerResponses = _swaggerResponses s <> _swaggerResponses t,
            _swaggerSecurityDefinitions = _swaggerSecurityDefinitions s <> _swaggerSecurityDefinitions t,
            _swaggerSecurity = _swaggerSecurity s <> _swaggerSecurity t,
            _swaggerTags = _swaggerTags s <> _swaggerTags t,
            _swaggerExternalDocs = _swaggerExternalDocs s <|> _swaggerExternalDocs t
          }

instance (KnownNat status, KnownStatus status) => HasStatus (ErrorDescription status desc) where
  type StatusOf (ErrorDescription status desc) = status

type ConversationNotFound = ErrorDescription 404 "Conversation not found"

convNotFound :: ConversationNotFound
convNotFound = ErrorDescription "no-conversation" "conversation not found"

type UnknownClient = ErrorDescription 403 "Unknown Client"

unknownClient :: UnknownClient
unknownClient = ErrorDescription "unknown-client" "Sending client not known"

type PostOtrResponses =
  '[ WithStatus 201 Public.ClientMismatch,
     WithStatus 412 Public.ClientMismatch,
     ConversationNotFound,
     UnknownClient
   ]

-- FUTUREWORK: Make a PR to the servant-swagger package with this instance
instance Swagger.ToSchema Servant.NoContent where
  declareNamedSchema _ = Swagger.declareNamedSchema (Proxy @())

data Api routes = Api
  { -- Conversations

    getConversation ::
      routes
        :- Summary "Get a conversation by ID"
        :> ZUser
        :> "conversations"
        :> Capture "cnv" ConvId
        :> Get '[Servant.JSON] Public.Conversation,
    getConversationRoles ::
      routes
        :- Summary "Get existing roles available for the given conversation"
        :> ZUser
        :> "conversations"
        :> Capture "cnv" ConvId
        :> "roles"
        :> Get '[Servant.JSON] Public.ConversationRolesList,
    getConversationIds ::
      routes
        :- Summary "Get all conversation IDs."
        -- FUTUREWORK: add bounds to swagger schema for Range
        :> ZUser
        :> "conversations"
        :> "ids"
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Conversation ID to start from (exclusive)"
             ]
             "start"
             ConvId
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Maximum number of IDs to return"
             ]
             "size"
             (Range 1 1000 Int32)
        :> Get '[Servant.JSON] (Public.ConversationList ConvId),
    getConversations ::
      routes
        :- Summary "Get all conversations"
        :> ZUser
        :> "conversations"
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Mutually exclusive with 'start' (at most 32 IDs per request)"
             ]
             "ids"
             (Range 1 32 (CommaSeparatedList ConvId))
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Conversation ID to start from (exclusive)"
             ]
             "start"
             ConvId
        :> QueryParam'
             [ Optional,
               Strict,
               Description "Maximum number of conversations to return"
             ]
             "size"
             (Range 1 500 Int32)
        :> Get '[Servant.JSON] (Public.ConversationList Public.Conversation),
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    -- FUTUREWORK: errorResponse Error.notConnected
    --             errorResponse Error.notATeamMember
    --             errorResponse (Error.operationDenied Public.CreateConversation)
    createGroupConversation ::
      routes
        :- Summary "Create a new conversation"
        :> Description "This returns 201 when a new conversation is created, and 200 when the conversation already existed"
        :> ZUser
        :> ZConn
        :> "conversations"
        :> ReqBody '[Servant.JSON] Public.NewConvUnmanaged
        :> UVerb 'POST '[Servant.JSON] ConversationResponses,
    createSelfConversation ::
      routes
        :- Summary "Create a self-conversation"
        :> ZUser
        :> "conversations"
        :> "self"
        :> UVerb 'POST '[Servant.JSON] ConversationResponses,
    -- This endpoint can lead to the following events being sent:
    -- - ConvCreate event to members
    -- TODO: add note: "On 201, the conversation ID is the `Location` header"
    createOne2OneConversation ::
      routes
        :- Summary "Create a 1:1 conversation"
        :> ZUser
        :> ZConn
        :> "conversations"
        :> "one2one"
        :> ReqBody '[Servant.JSON] Public.NewConvUnmanaged
        :> UVerb 'POST '[Servant.JSON] ConversationResponses,
    addMembersToConversationV2 ::
      routes
        :- Summary "Add qualified members to an existing conversation: WIP, inaccessible for clients until ready"
        :> ZUser
        :> ZConn
        :> "i" -- FUTUREWORK: remove this /i/ once it's ready. See comment on 'Update.addMembers'
        :> "conversations"
        :> Capture "cnv" ConvId
        :> "members"
        :> "v2"
        :> ReqBody '[Servant.JSON] Public.InviteQualified
        :> UVerb 'POST '[Servant.JSON] UpdateResponses,
    -- Team Conversations

    getTeamConversationRoles ::
      -- FUTUREWORK: errorResponse Error.notATeamMember
      routes
        :- Summary "Get existing roles available for the given team"
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> "roles"
        :> Get '[Servant.JSON] Public.ConversationRolesList,
    -- FUTUREWORK: errorResponse (Error.operationDenied Public.GetTeamConversations)
    getTeamConversations ::
      routes
        :- Summary "Get team conversations"
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> Get '[Servant.JSON] Public.TeamConversationList,
    -- FUTUREWORK: errorResponse (Error.operationDenied Public.GetTeamConversations)
    getTeamConversation ::
      routes
        :- Summary "Get one team conversation"
        :> ZUser
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> Capture "cid" ConvId
        :> Get '[Servant.JSON] Public.TeamConversation,
    -- FUTUREWORK: errorResponse (Error.actionDenied Public.DeleteConversation)
    --             errorResponse Error.notATeamMember
    deleteTeamConversation ::
      routes
        :- Summary "Remove a team conversation"
        :> ZUser
        :> ZConn
        :> "teams"
        :> Capture "tid" TeamId
        :> "conversations"
        :> Capture "cid" ConvId
        :> Delete '[] (EmptyResult 200),
    -- | This endpoint can lead to the following events being sent:
    --
    -- - OtrMessageAdd event to recipients
    --
    -- TODO: Add 404 for conv not found
    -- TODO: Add 403 for unknown sending client
    postOtrMessage ::
      routes
        :- Summary "Post an encrypted message to a conversation (accepts JSON)"
        :> ZUser
        :> ZConn
        :> "conversations"
        :> Capture "cnv" ConvId
        :> QueryParam "ignore_missing" IgnoreMissing
        :> QueryParam "report_missing" ReportMissing
        :> "otr"
        :> "messages"
        :> ReqBody '[Servant.JSON] Public.NewOtrMessage
        :> UVerb 'POST '[Servant.JSON] PostOtrResponses
  }
  deriving (Generic)

type ServantAPI = ToServantApi Api

data IgnoreMissing
  = IgnoreMissingAll
  | IgnoreMissingList (Set UserId)
  deriving (Show, Eq)

-- TODO: Fill this in
instance Swagger.ToParamSchema IgnoreMissing where
  toParamSchema _ = mempty

-- TODO: Test what happens when empty string is sent, is it backwards compatible?
-- TODO: Test what happens when true and false have different cases, is it backwards compatible?
instance FromHttpApiData IgnoreMissing where
  parseQueryParam = \case
    "true" -> Right IgnoreMissingAll
    "false" -> Right $ IgnoreMissingList mempty
    list -> IgnoreMissingList . Set.fromList . fromCommaSeparatedList <$> parseQueryParam list

data ReportMissing
  = ReportMissingAll
  | ReportMissingList (Set UserId)

instance Swagger.ToParamSchema ReportMissing where
  toParamSchema _ = mempty

-- TODO: Test what happens when empty string is sent, is it backwards compatible?
-- TODO: Test what happens when true and false have different cases, is it backwards compatible?
instance FromHttpApiData ReportMissing where
  parseQueryParam = \case
    "true" -> Right ReportMissingAll
    "false" -> Right $ ReportMissingList mempty
    list -> ReportMissingList . Set.fromList . fromCommaSeparatedList <$> parseQueryParam list

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy @ServantAPI)

-- post "/conversations/:cnv/otr/messages" (continue Update.postOtrMessageH) $
--     zauthUserId
--       .&. zauthConnId
--       .&. capture "cnv"
--       .&. def Public.OtrReportAllMissing filterMissing
--       .&. jsonRequest @Public.NewOtrMessage
--   document "POST" "postOtrMessage" $ do
--     summary "Post an encrypted message to a conversation (accepts JSON)"
--     parameter Path "cnv" bytes' $
--       description "Conversation ID"
--     parameter Query "ignore_missing" bool' $ do
--       description
--         "Force message delivery even when clients are missing. \
--         \NOTE: can also be a comma-separated list of user IDs, \
--         \in which case it specifies who exactly is allowed to \
--         \have missing clients."
--       optional
--     parameter Query "report_missing" bool' $ do
--       description
--         "Don't allow message delivery when clients are missing \
--         \('ignore_missing' takes precedence when present). \
--         \NOTE: can also be a comma-separated list of user IDs, \
--         \in which case it specifies who exactly is forbidden from \
--         \having missing clients. \
--         \To support large lists of user IDs exceeding the allowed \
--         \URL length, you can also put this list in the body, in \
--         \the optional field 'report_missing'.  That body field takes \
--         \prhttps://wearezeta.atlassian.net/wiki/spaces/ENGINEERIN/pages/376439791/Use%2Bcase%2BClassified%2Bdomains?focusedCommentId=384861252#comment-384861252ecedence over both query params."
--       optional
--     body (ref Public.modelNewOtrMessage) $
--       description "JSON body"
--     returns (ref Public.modelClientMismatch)
--     response 201 "Message posted" end
--     response 412 "Missing clients" end
--     errorResponse Error.convNotFound
--     errorResponse Error.unknownClient
