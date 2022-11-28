module Brig.API.Public.Swagger
  ( VersionedSwaggerDocsAPI,
    DocsAPI,
    EventNotificationsAPI,
    pregenSwagger,
    swaggerPregenUIServer,
    eventNotificationsAPI,
  )
where

import Control.Lens ((.~))
import qualified Data.Aeson as A
import Data.FileEmbed
import qualified Data.HashMap.Strict.InsOrd as HM
import Data.Swagger (NamedSchema (NamedSchema))
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as T
import FileEmbedLzma
import Imports hiding (head)
import Language.Haskell.TH
import Servant
import Servant.Swagger
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import qualified Wire.API.Event.Conversation
import qualified Wire.API.Event.FeatureConfig
import qualified Wire.API.Event.Team
import Wire.API.Routes.Version
import Wire.API.SwaggerHelper (cleanupSwagger)

type VersionedSwaggerDocsAPIBase = SwaggerSchemaUI "swagger-ui" "swagger.json"

type VersionedSwaggerDocsAPI = "api" :> Header VersionHeader Version :> VersionedSwaggerDocsAPIBase

type NotificationSchemasAPI = "api" :> "event-notification-schemas" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

type DocsAPI = VersionedSwaggerDocsAPI :<|> NotificationSchemasAPI

data EventNotificationsAPI

instance HasSwagger EventNotificationsAPI where
  toSwagger _ =
    mempty
      & S.definitions .~ eventNotificationSchemas
      & S.info . S.title .~ "Wire-Server Event Definitions"
      & cleanupSwagger

eventNotificationsAPI :: S.Swagger
eventNotificationsAPI = toSwagger (Proxy @EventNotificationsAPI)

pregenSwagger :: Version -> Q Exp
pregenSwagger v =
  embedLazyByteString
    =<< makeRelativeToProject
      ("docs/swagger-v" <> T.unpack (toUrlPiece v) <> ".json")

swaggerPregenUIServer :: LByteString -> Server VersionedSwaggerDocsAPIBase
swaggerPregenUIServer =
  swaggerSchemaUIServer
    . fromMaybe A.Null
    . A.decode

{- FUTUREWORK(fisx): there are a few things that need to be fixed before this schema collection
   is of any practical use!

- The different `EventData` constructors all have different json schema that partially
  overlap.  Our schemas only represent the union of all those constructors, rather than a
  list of cases.  There may be a better way even in swagger v2:
  https://swagger.io/specification/v2/ (look for "polymorphism")

- Document how this works in
  https://docs.wire.com/understand/api-client-perspective/swagger.html

tracked in https://wearezeta.atlassian.net/browse/FS-1008 -}
eventNotificationSchemas :: S.Definitions S.Schema
eventNotificationSchemas = fst . (`S.runDeclare` mempty) $ renderAll
  where
    renderAll :: S.Declare (S.Definitions S.Schema) ()
    renderAll = do
      render @Wire.API.Event.Conversation.Event
      render @Wire.API.Event.FeatureConfig.Event
      render @Wire.API.Event.Team.Event
      render @Wire.API.Event.Team.EventData
      render @Wire.API.Event.Conversation.EventData

    render :: forall a. S.ToSchema a => S.Declare (S.Definitions S.Schema) ()
    render = do
      NamedSchema mName eventSchema <- S.declareNamedSchema (Proxy @a)
      for_ mName $ \name -> S.declare (HM.singleton name eventSchema)
