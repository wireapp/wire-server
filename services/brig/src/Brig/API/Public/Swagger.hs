module Brig.API.Public.Swagger
  ( VersionedSwaggerDocsAPI,
    InternalEndpointsSwaggerDocsAPI,
    DocsAPI,
    pregenSwagger,
    swaggerPregenUIServer,
    eventNotificationSchemas,
  )
where

import Control.Lens
import qualified Data.Aeson as A
import Data.FileEmbed
import qualified Data.HashMap.Strict.InsOrd as HM
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as T
import FileEmbedLzma
import Imports hiding (head)
import Language.Haskell.TH
import Servant
import Servant.Swagger.Internal.Orphans ()
import Servant.Swagger.UI
import qualified Wire.API.Event.Conversation
import qualified Wire.API.Event.FeatureConfig
import qualified Wire.API.Event.Team
import Wire.API.Routes.Version

type VersionedSwaggerDocsAPIBase = SwaggerSchemaUI "swagger-ui" "swagger.json"

type VersionedSwaggerDocsAPI = "api" :> Header VersionHeader Version :> VersionedSwaggerDocsAPIBase

type InternalEndpointsSwaggerDocsAPI = "api-internal" :> VersionedSwaggerDocsAPIBase

type NotificationSchemasAPI = "api" :> "event-notification-schemas" :> Get '[JSON] [S.Definitions S.Schema]

type DocsAPI = VersionedSwaggerDocsAPI :<|> NotificationSchemasAPI :<|> InternalEndpointsSwaggerDocsAPI

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
