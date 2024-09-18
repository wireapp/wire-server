-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Gundeck.API.Internal
  ( type GundeckInternalAPI,
    servantSitemap,
  )
where

import Cassandra qualified
import Control.Lens (view)
import Data.Aeson (eitherDecode)
import Data.CommaSeparatedList
import Data.Id
import Data.Metrics.Servant
import Data.Typeable
import Gundeck.Client qualified as Client
import Gundeck.Monad
import Gundeck.Presence qualified as Presence
import Gundeck.Push qualified as Push
import Gundeck.Push.Data qualified as PushTok
import Gundeck.Push.Native.Types qualified as PushTok
import Gundeck.Types.Presence as GD
import Gundeck.Types.Push.V2
import Imports
import Network.Wai (lazyRequestBody)
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.ErrorFormatter
import Wire.API.Push.Token qualified as PushTok
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

-- | cloned from instance for ReqBody'.
instance
  (RoutesToPaths rest) =>
  RoutesToPaths (ReqBodyHack :> rest)
  where
  getRoutes = getRoutes @rest

type GundeckInternalAPI =
  "i"
    :> ( ("status" :> Get '[JSON] NoContent)
           :<|> ("push" :> "v2" :> ReqBody '[JSON] [Push] :> Post '[JSON] NoContent)
           :<|> ( "presences"
                    :> ( (QueryParam' [Required, Strict] "ids" (CommaSeparatedList UserId) :> Get '[JSON] [Presence])
                           :<|> (Capture "uid" UserId :> Get '[JSON] [Presence])
                           :<|> (ReqBodyHack :> Verb 'POST 201 '[JSON] (Headers '[Header "Location" GD.URI] NoContent))
                           :<|> (Capture "uid" UserId :> "devices" :> Capture "did" ConnId :> "cannons" :> Capture "cannon" CannonId :> Delete '[JSON] NoContent)
                       )
                )
           :<|> (ZUser :> "clients" :> Capture "cid" ClientId :> Delete '[JSON] NoContent)
           :<|> (ZUser :> "user" :> Delete '[JSON] NoContent)
           :<|> ("push-tokens" :> Capture "uid" UserId :> Get '[JSON] PushTokenList)
       )

servantSitemap :: ServerT GundeckInternalAPI Gundeck
servantSitemap =
  statusH
    :<|> pushH
    :<|> ( Presence.listAllH
             :<|> Presence.listH
             :<|> Presence.addH
             :<|> Presence.removeH
         )
    :<|> unregisterClientH
    :<|> removeUserH
    :<|> getPushTokensH

statusH :: (Applicative m) => m NoContent
statusH = pure NoContent

pushH :: [Push] -> Gundeck NoContent
pushH ps = NoContent <$ Push.push ps

unregisterClientH :: UserId -> ClientId -> Gundeck NoContent
unregisterClientH uid cid = NoContent <$ Client.unregister uid cid

removeUserH :: UserId -> Gundeck NoContent
removeUserH uid = NoContent <$ Client.removeUser uid

getPushTokensH :: UserId -> Gundeck PushTok.PushTokenList
getPushTokensH uid = PushTok.PushTokenList <$> (view PushTok.addrPushToken <$$> PushTok.lookup uid Cassandra.All)
