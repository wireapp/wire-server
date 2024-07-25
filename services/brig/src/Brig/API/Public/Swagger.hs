module Brig.API.Public.Swagger
  ( VersionedSwaggerDocsAPI,
    InternalEndpointsSwaggerDocsAPI,
    VersionedSwaggerDocsAPIBase,
    SwaggerDocsAPIBase,
    ServiceSwaggerDocsAPIBase,
    DocsAPI,
    FederationSwaggerDocsAPI,
    pregenSwagger,
    swaggerPregenUIServer,
    eventNotificationSchemas,
    adjustSwaggerForInternalEndpoint,
    adjustSwaggerForFederationEndpoints,
    emptySwagger,
  )
where

import Control.Lens
import Data.Aeson qualified as A
import Data.FileEmbed
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.HashSet.InsOrd qualified as InsOrdSet
import Data.Kind qualified as Kind
import Data.OpenApi qualified as S
import Data.OpenApi.Declare qualified as S
import Data.Text qualified as T
import FileEmbedLzma
import GHC.TypeLits
import Imports hiding (head)
import Language.Haskell.TH
import Network.Socket
import Servant
import Servant.OpenApi.Internal.Orphans ()
import Servant.Swagger.UI
import Wire.API.Event.Conversation qualified
import Wire.API.Event.FeatureConfig qualified
import Wire.API.Event.Team qualified
import Wire.API.Routes.Version

type SwaggerDocsAPIBase = SwaggerSchemaUI "swagger-ui" "swagger.json"

type VersionedSwaggerDocsAPI = "api" :> Header VersionHeader VersionNumber :> SwaggerDocsAPIBase

type ServiceSwaggerDocsAPIBase :: Symbol -> Kind.Type
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

type NotificationSchemasAPI = "api" :> "event-notification-schemas" :> Get '[JSON] [S.Definitions S.Schema]

type FederationSwaggerDocsAPI =
  "api-federation"
    :> "swagger-ui"
    :> ( ServiceSwaggerDocsAPIBase "brig"
           :<|> ServiceSwaggerDocsAPIBase "galley"
           :<|> ServiceSwaggerDocsAPIBase "cargohold"
       )

type DocsAPI =
  VersionedSwaggerDocsAPI
    :<|> NotificationSchemasAPI
    :<|> InternalEndpointsSwaggerDocsAPI
    :<|> FederationSwaggerDocsAPI

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

adjustSwaggerForInternalEndpoint :: String -> PortNumber -> S.OpenApi -> S.OpenApi
adjustSwaggerForInternalEndpoint service examplePort swagger =
  swagger
    & S.info . S.title .~ T.pack ("Wire-Server Internal API (" ++ service ++ ")")
    & S.info . S.description ?~ renderedDescription
    & S.allOperations . S.tags <>~ tag
    -- Enforce HTTP as the services themselves don't understand HTTPS
    & S.servers .~ [S.Server ("http://localhost:" <> T.pack (show examplePort)) Nothing mempty]
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

adjustSwaggerForFederationEndpoints :: String -> S.OpenApi -> S.OpenApi
adjustSwaggerForFederationEndpoints service swagger =
  swagger
    & S.info . S.title .~ T.pack ("Wire-Server Federation API (" ++ service ++ ")")
    & S.allOperations . S.tags <>~ tag
  where
    tag :: InsOrdSet.InsOrdHashSet S.TagName
    tag = InsOrdSet.singleton @S.TagName (T.pack service)

emptySwagger :: Servant.Server (ServiceSwaggerDocsAPIBase a)
emptySwagger =
  swaggerSchemaUIServer $
    mempty @S.OpenApi
      & S.info . S.description
        ?~ "There is no Swagger documentation for this version. Please refer to v5 or later."

eventNotificationSchemas :: [S.Definitions S.Schema]
eventNotificationSchemas = fst . (`S.runDeclare` mempty) <$> renderAll
  where
    renderAll :: [S.Declare (S.Definitions S.Schema) ()]
    renderAll =
      [ render @Wire.API.Event.Conversation.Event "Wire.API.Event.Conversation.Event",
        render @Wire.API.Event.FeatureConfig.Event "Wire.API.Event.FeatureConfig.Event",
        render @Wire.API.Event.Team.Event "Wire.API.Event.Team.Event"
      ]

    render :: forall a. (S.ToSchema a) => Text -> S.Declare (S.Definitions S.Schema) ()
    render eventTypeName = do
      eventSchema <- S.declareNamedSchema (Proxy @a) <&> view S.schema
      S.declare (HM.singleton eventTypeName eventSchema)
