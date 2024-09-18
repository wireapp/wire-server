module Wire.API.Routes.Internal.Gundeck where

import Control.Lens ((.~))
import Data.Aeson
import Data.CommaSeparatedList
import Data.Id
import Data.OpenApi hiding (HasServer, Header)
import Data.Typeable
import Imports
import Network.URI
import Network.Wai
import Servant
import Servant.OpenApi
import Servant.Server
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.ErrorFormatter
import Wire.API.CannonId
import Wire.API.Presence
import Wire.API.Push.V2
import Wire.API.Push.V2.Token
import Wire.API.Routes.Public

-- | this can be replaced by `ReqBody '[JSON] Presence` once the fix in cannon from
-- https://github.com/wireapp/wire-server/pull/4246 has been deployed everywhere.
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

type InternalAPI =
  "i"
    :> ( ("status" :> Get '[JSON] NoContent)
           :<|> ("push" :> "v2" :> ReqBody '[JSON] [Push] :> Post '[JSON] NoContent)
           :<|> ( "presences"
                    :> ( (QueryParam' [Required, Strict] "ids" (CommaSeparatedList UserId) :> Get '[JSON] [Presence])
                           :<|> (Capture "uid" UserId :> Get '[JSON] [Presence])
                           :<|> (ReqBodyHack :> Post '[JSON] (Headers '[Header "Location" URI] NoContent))
                           :<|> (Capture "uid" UserId :> "devices" :> Capture "did" ConnId :> "cannons" :> Capture "cannon" CannonId :> Delete '[JSON] NoContent)
                       )
                )
           :<|> (ZUser :> "clients" :> Capture "cid" ClientId :> Delete '[JSON] NoContent)
           :<|> (ZUser :> "user" :> Delete '[JSON] NoContent)
           :<|> ("push-tokens" :> Capture "uid" UserId :> Get '[JSON] PushTokenList)
       )

swaggerDoc :: OpenApi
swaggerDoc =
  toOpenApi (Proxy @InternalAPI)
    & info . title .~ "Wire-Server internal gundeck API"
