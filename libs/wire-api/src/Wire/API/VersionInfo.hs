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

module Wire.API.VersionInfo
  ( -- * Version info
    vinfoObjectSchema,

    -- * Version utilities
    versionHeader,
    VersionHeader,

    -- * Servant combinators
    From,
    Until,
    VersionedMonad (..),
  )
where

import Data.ByteString.Char8 qualified as B8
import Data.CaseInsensitive qualified as CI
import Data.Metrics.Servant
import Data.Schema
import Data.Singletons
import GHC.TypeLits
import Imports
import Network.Wai qualified as Wai
import Servant
import Servant.Client.Core
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Wire.API.Routes.ClientAlgebra

vinfoObjectSchema :: ValueSchema NamedSwaggerDoc v -> ObjectSchema SwaggerDoc [v]
vinfoObjectSchema sch = field "supported" (array sch)

type VersionHeader = "X-Wire-API-Version"

versionHeader :: CI.CI ByteString
versionHeader = CI.mk . B8.pack $ symbolVal (Proxy @VersionHeader)

--------------------------------------------------------------------------------
-- Servant combinators

data Until v

data From v

instance
  ( SingI n,
    Ord (Demote v),
    Enum (Demote v),
    SingKind v,
    HasServer api ctx
  ) =>
  HasServer (Until (n :: v) :> api) ctx
  where
  type ServerT (Until n :> api) m = ServerT api m

  route _ ctx action =
    route (Proxy @api) ctx $
      fmap const action `addHeaderCheck` withRequest headerCheck
    where
      headerCheck :: Wai.Request -> DelayedIO ()
      headerCheck req = do
        let v =
              toEnum $
                maybe
                  0
                  (fromRight 0 . parseHeader)
                  (lookup versionHeader (Wai.requestHeaders req))
        when (v >= demote @n) $
          delayedFail err404

  hoistServerWithContext _ ctx f =
    hoistServerWithContext (Proxy @api) ctx f

class VersionedMonad v m where
  guardVersion :: (v -> Bool) -> m ()

instance
  ( VersionedMonad (Demote v) m,
    SingI n,
    Ord (Demote v),
    SingKind v,
    HasClientAlgebra m api
  ) =>
  HasClient m (Until (n :: v) :> api)
  where
  type Client m (Until n :> api) = Client m api
  clientWithRoute pm _ req = bindClient @m @api
    (guardVersion (\v -> v < demote @n))
    $ \_ ->
      clientWithRoute pm (Proxy @api) req
  hoistClientMonad pm _ f = hoistClientMonad pm (Proxy @api) f

instance (RoutesToPaths api) => RoutesToPaths (Until v :> api) where
  getRoutes = getRoutes @api

instance
  ( SingI n,
    Ord (Demote v),
    Enum (Demote v),
    SingKind v,
    HasServer api ctx
  ) =>
  HasServer (From (n :: v) :> api) ctx
  where
  type ServerT (From n :> api) m = ServerT api m

  route _ ctx action =
    route (Proxy @api) ctx $
      fmap const action `addHeaderCheck` withRequest headerCheck
    where
      headerCheck :: Wai.Request -> DelayedIO ()
      headerCheck req = do
        let v =
              toEnum $
                maybe
                  0
                  (fromRight 0 . parseHeader)
                  (lookup versionHeader (Wai.requestHeaders req))
        when (v < demote @n) $
          delayedFail err404

  hoistServerWithContext _ ctx f =
    hoistServerWithContext (Proxy @api) ctx f

instance
  ( VersionedMonad (Demote v) m,
    SingI n,
    Ord (Demote v),
    SingKind v,
    HasClientAlgebra m api
  ) =>
  HasClient m (From (n :: v) :> api)
  where
  type Client m (From n :> api) = Client m api
  clientWithRoute pm _ req = bindClient @m @api
    (guardVersion (\v -> v >= demote @n))
    $ \_ ->
      clientWithRoute pm (Proxy @api) req
  hoistClientMonad pm _ f = hoistClientMonad pm (Proxy @api) f

instance (RoutesToPaths api) => RoutesToPaths (From v :> api) where
  getRoutes = getRoutes @api
