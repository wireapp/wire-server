module Wire.API.Routes.Internal.Galley.ConversationsIntra
  ( DesiredMembership (..),
    Actor (..),
    UpsertOne2OneConversationRequest (..),
    UpsertOne2OneConversationResponse (..),
  )
where

import qualified Data.Aeson as A
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Id (ConvId, UserId)
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as Swagger
import Imports

data DesiredMembership = Included | Excluded
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Schema DesiredMembership

instance ToSchema DesiredMembership where
  schema =
    enum @Text "DesiredMembership" $
      mconcat
        [ element "included" Included,
          element "excluded" Excluded
        ]

data Actor = LocalActor | RemoteActor
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via Schema Actor

instance ToSchema Actor where
  schema =
    enum @Text "Actor" $
      mconcat
        [ element "local_actor" LocalActor,
          element "remote_actor" RemoteActor
        ]

data UpsertOne2OneConversationRequest = UpsertOne2OneConversationRequest
  { uooLocalUser :: Local UserId,
    uooRemoteUser :: Remote UserId,
    uooActor :: Actor,
    uooActorDesiredMembership :: DesiredMembership,
    uooConvId :: Maybe (Qualified ConvId)
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema UpsertOne2OneConversationRequest

instance ToSchema UpsertOne2OneConversationRequest where
  schema =
    object "UpsertOne2OneConversationRequest" $
      UpsertOne2OneConversationRequest
        <$> (tUntagged . uooLocalUser) .= field "local_user" (qTagUnsafe <$> schema)
        <*> (tUntagged . uooRemoteUser) .= field "remote_user" (qTagUnsafe <$> schema)
        <*> uooActor .= field "actor" schema
        <*> uooActorDesiredMembership .= field "actor_desired_membership" schema
        <*> uooConvId .= optField "conversation_id" (maybeWithDefault A.Null schema)

newtype UpsertOne2OneConversationResponse = UpsertOne2OneConversationResponse
  { uuorConvId :: Qualified ConvId
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via Schema UpsertOne2OneConversationResponse

instance ToSchema UpsertOne2OneConversationResponse where
  schema =
    object "UpsertOne2OneConversationResponse" $
      UpsertOne2OneConversationResponse
        <$> uuorConvId .= field "conversation_id" schema
