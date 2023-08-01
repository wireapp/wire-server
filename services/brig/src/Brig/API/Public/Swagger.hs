module Brig.API.Public.Swagger
  ( VersionedSwaggerDocsAPI,
    InternalEndpointsSwaggerDocsAPI,
    VersionedSwaggerDocsAPIBase,
    SwaggerDocsAPIBase,
    ServiceSwaggerDocsAPIBase,
    DocsAPI,
    EventNotificationsAPI,
    pregenSwagger,
    swaggerPregenUIServer,
    eventNotificationsAPI,
    eventNotificationSchemas,
    adjustSwaggerForInternalEndpoint,
    emptySwagger,
  )
where

import Control.Lens
import qualified Data.Aeson as A
import Data.FileEmbed
import qualified Data.HashMap.Strict.InsOrd as HM
import qualified Data.HashSet.InsOrd as InsOrdSet
import Data.Swagger (NamedSchema (NamedSchema))
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as T
import FileEmbedLzma
import GHC.TypeLits
import Imports hiding (head)
import Language.Haskell.TH
import Network.Socket
import Servant
import Servant.Swagger
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import qualified Wire.API.Event.Conversation
import qualified Wire.API.Event.FeatureConfig
import qualified Wire.API.Event.Team
import Wire.API.Routes.Version
import Wire.API.SwaggerHelper (cleanupSwagger)

type SwaggerDocsAPIBase = SwaggerSchemaUI "swagger-ui" "swagger.json"

type VersionedSwaggerDocsAPI = "api" :> Header VersionHeader VersionNumber :> SwaggerDocsAPIBase

type ServiceSwaggerDocsAPIBase service = SwaggerSchemaUI service (AppendSymbol service "-swagger.json")

type VersionedSwaggerDocsAPIBase service = Header VersionHeader VersionNumber :> ServiceSwaggerDocsAPIBase service

type InternalEndpointsSwaggerDocsAPI =
  "api-internal"
    :> "swagger-ui"
    :> ( VersionedSwaggerDocsAPIBase "brig"
           :<|> VersionedSwaggerDocsAPIBase "cannon"
           :<|> VersionedSwaggerDocsAPIBase "cargohold"
           :<|> VersionedSwaggerDocsAPIBase "galley"
           :<|> VersionedSwaggerDocsAPIBase "spar"
       )

type NotificationSchemasAPI = "api" :> "event-notification-schemas" :> SwaggerSchemaUI "swagger-ui" "swagger.json"

type DocsAPI = VersionedSwaggerDocsAPI :<|> NotificationSchemasAPI :<|> InternalEndpointsSwaggerDocsAPI

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
      ("docs/swagger-v" <> T.unpack (toUrlPiece (VersionNumber v)) <> ".json")

swaggerPregenUIServer :: LByteString -> Server SwaggerDocsAPIBase
swaggerPregenUIServer =
  swaggerSchemaUIServer
    . fromMaybe A.Null
    . A.decode

adjustSwaggerForInternalEndpoint :: String -> PortNumber -> S.Swagger -> S.Swagger
adjustSwaggerForInternalEndpoint service examplePort swagger =
  swagger
    & S.info . S.title .~ T.pack ("Wire-Server internal API (" ++ service ++ ")")
    & S.info . S.description ?~ renderedDescription
    & S.host ?~ S.Host "localhost" (Just examplePort)
    & S.allOperations . S.tags <>~ tag
    -- Enforce HTTP as the services themselves don't understand HTTPS
    & S.schemes ?~ [S.Http]
    & S.allOperations . S.schemes ?~ [S.Http]
  where
    tag :: InsOrdSet.InsOrdHashSet S.TagName
    tag = InsOrdSet.singleton @S.TagName (T.pack service)

    renderedDescription :: Text
    renderedDescription =
      T.pack . Imports.unlines $
        [ "To have access to this *internal* endpoint, create a port forwarding to `"
            ++ service
            ++ "` into the Kubernetes cluster. E.g.:",
          "```",
          "kubectl port-forward -n wire service/"
            ++ service
            ++ " "
            ++ show examplePort
            ++ ":8080",
          "```",
          "**N.B.:** Execution via this UI won't work due to CORS issues."
            ++ " But, the proposed `curl` commands will."
        ]

emptySwagger :: Servant.Server (ServiceSwaggerDocsAPIBase a)
emptySwagger =
  swaggerSchemaUIServer $
    mempty @S.Swagger
      & S.info . S.description
        ?~ "There is no Swagger documentation for this version. Please refer to v3 or later."

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
