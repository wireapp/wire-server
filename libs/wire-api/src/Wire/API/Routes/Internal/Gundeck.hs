module Wire.API.Routes.Internal.Gundeck where

import Control.Lens ((%~), (.~), (?~))
import Data.Aeson
import Data.CommaSeparatedList
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Id
import Data.Metrics.Servant
import Data.OpenApi qualified as S hiding (HasServer, Header)
import Data.OpenApi.Declare qualified as S
import Data.Text qualified as Text
import Data.Typeable
import Imports
import Network.Wai
import Servant hiding (URI (..))
import Servant.API.Description
import Servant.OpenApi
import Servant.OpenApi.Internal
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.ErrorFormatter
import Wire.API.CannonId
import Wire.API.Presence
import Wire.API.Push.V2
import Wire.API.Routes.Named
import Wire.API.Routes.Public

-- | this can be replaced by `ReqBody '[JSON] Presence` once the fix in cannon from
-- https://github.com/wireapp/wire-server/pull/4246 has been deployed everywhere.
--
-- Background: Cannon.WS.regInfo called gundeck without setting the content-type header here.
-- wai-routes and wai-predicates were able to work with that; servant is less lenient.
data ReqBodyHack

-- | cloned from instance for ReqBody'.
instance
  ( HasServer api context,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasServer (ReqBodyHack :> api) context
  where
  type ServerT (ReqBodyHack :> api) m = Presence -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context $
      addBodyCheck subserver ctCheck bodyCheck
    where
      rep = typeRep (Proxy :: Proxy ReqBodyHack)
      formatError = bodyParserErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      ctCheck = pure eitherDecode

      -- Body check, we get a body parsing functions as the first argument.
      bodyCheck f = withRequest $ \request -> do
        mrqbody <- f <$> liftIO (lazyRequestBody request)
        case mrqbody of
          Left e -> delayedFailFatal $ formatError rep request e
          Right v -> pure v

-- | cloned from instance for ReqBody'.
instance (RoutesToPaths route) => RoutesToPaths (ReqBodyHack :> route) where
  getRoutes = getRoutes @route

-- | cloned from instance for ReqBody'.
instance (HasOpenApi sub) => HasOpenApi (ReqBodyHack :> sub) where
  toOpenApi _ =
    toOpenApi (Proxy @sub)
      & addRequestBody reqBody
      & addDefaultResponse400 tname
      & S.components . S.schemas %~ (<> defs)
    where
      tname = "body"
      transDesc "" = Nothing
      transDesc desc = Just (Text.pack desc)
      (defs, ref) = S.runDeclare (S.declareSchemaRef (Proxy @Presence)) mempty
      reqBody =
        (mempty :: S.RequestBody)
          & S.description .~ transDesc (reflectDescription (Proxy :: Proxy '[]))
          & S.required ?~ True
          & S.content .~ InsOrdHashMap.fromList [(t, mempty & S.schema ?~ ref) | t <- allContentType (Proxy :: Proxy '[JSON])]

type InternalAPI =
  "i"
    :> ( Named "i-status" ("status" :> Get '[JSON] NoContent)
           :<|> Named "i-push" ("push" :> "v2" :> ReqBody '[JSON] [Push] :> Post '[JSON] NoContent)
           :<|> ( "presences"
                    :> ( Named "i-presences-get-for-users" (QueryParam' [Required, Strict] "ids" (CommaSeparatedList UserId) :> Get '[JSON] [Presence])
                           :<|> Named "i-presences-get-for-user" (Capture "uid" UserId :> Get '[JSON] [Presence])
                           :<|> Named "i-presences-post" (ReqBodyHack :> Verb 'POST 201 '[JSON] (Headers '[Header "Location" URI] NoContent))
                           :<|> Named "i-presences-delete" (Capture "uid" UserId :> "devices" :> Capture "did" ConnId :> "cannons" :> Capture "cannon" CannonId :> Delete '[JSON] NoContent)
                       )
                )
           :<|> Named "i-clients-delete" (ZUser :> "clients" :> Capture "cid" ClientId :> Delete '[JSON] NoContent)
           :<|> Named "i-user-delete" (ZUser :> "user" :> Delete '[JSON] NoContent)
           :<|> Named "i-push-tokens-get" ("push-tokens" :> Capture "uid" UserId :> Get '[JSON] PushTokenList)
       )

swaggerDoc :: S.OpenApi
swaggerDoc =
  toOpenApi (Proxy @InternalAPI)
    & S.info . S.title .~ "Wire-Server internal gundeck API"
