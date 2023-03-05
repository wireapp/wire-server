module Brig.API.Public.Swagger
  ( VersionedSwaggerDocsAPI,
    InternalEndpointsSwaggerDocsAPI,
    VersionedSwaggerDocsAPIBase,
    SwaggerDocsAPIBase,
    ServiceSwaggerDocsAPIBase,
    DocsAPI,
    pregenSwagger,
    swaggerPregenUIServer,
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
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as T
import FileEmbedLzma
import GHC.TypeLits
import Imports hiding (head)
import Language.Haskell.TH
import Network.Socket
import Servant
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import qualified Wire.API.Event.Conversation
import qualified Wire.API.Event.FeatureConfig
import qualified Wire.API.Event.Team
import Wire.API.Routes.Version

type SwaggerDocsAPIBase = SwaggerSchemaUI "swagger-ui" "swagger.json"

type VersionedSwaggerDocsAPI = "api" :> Header VersionHeader Version :> SwaggerDocsAPIBase

type ServiceSwaggerDocsAPIBase service = SwaggerSchemaUI service (AppendSymbol service "-swagger.json")

type VersionedSwaggerDocsAPIBase service = Header VersionHeader Version :> ServiceSwaggerDocsAPIBase service

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

type DocsAPI = VersionedSwaggerDocsAPI :<|> NotificationSchemasAPI :<|> InternalEndpointsSwaggerDocsAPI

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

- `ToSchema` instances of team notifications are wrong.  To do this right, search
  schema-profunctor tutorial for bind/dispatch, and consult conversation events for
  examples.

- swagger2 doesn't handle types with the same name from different models well; it silently
  drops the second definition, which is what you want only if there are no name clashes as
  in our case with three types called `Event` and three types called `EventType`.  We have
  solved this by rendering the three event types seperately and returning each
  declarations list in a super-list.  For a better work-around, check
  https://github.com/GetShopTV/swagger2/issues/14.

- The different `EventData` constructors all have different json schema that partially
  overlap.  Our schemas only represent the union of all those constructors, rather than a
  list of cases.  There may be a better way even in swagger v2:
  https://swagger.io/specification/v2/ (look for "polymorphism")

- (wire cloud) expose end-point via nginz (only on staging).

- Document how this works in
  https://docs.wire.com/understand/api-client-perspective/swagger.html

tracked in https://wearezeta.atlassian.net/browse/FS-1008 -}
eventNotificationSchemas :: [S.Definitions S.Schema]
eventNotificationSchemas = fst . (`S.runDeclare` mempty) <$> renderAll
  where
    renderAll :: [S.Declare (S.Definitions S.Schema) ()]
    renderAll =
      [ render @Wire.API.Event.Conversation.Event "Wire.API.Event.Conversation.Event",
        render @Wire.API.Event.FeatureConfig.Event "Wire.API.Event.FeatureConfig.Event",
        render @Wire.API.Event.Team.Event "Wire.API.Event.Team.Event"
      ]

    render :: forall a. S.ToSchema a => Text -> S.Declare (S.Definitions S.Schema) ()
    render eventTypeName = do
      eventSchema <- S.declareNamedSchema (Proxy @a) <&> view S.schema
      S.declare (HM.singleton eventTypeName eventSchema)
