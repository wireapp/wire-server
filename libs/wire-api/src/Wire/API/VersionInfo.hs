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
    readVersionNumber,
    mkVersion,
    versionHeader,

    -- * Servant combinators
    From,
    Until,
  )
where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.CaseInsensitive as CI
import Data.Metrics.Servant
import Data.Schema
import Data.Singletons
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Imports
import qualified Network.Wai as Wai
import Servant
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Swagger

vinfoObjectSchema :: ValueSchema NamedSwaggerDoc v -> ObjectSchema SwaggerDoc [v]
vinfoObjectSchema sch = field "supported" (array sch)

readVersionNumber :: Text -> Maybe Integer
readVersionNumber v = do
  ('v', rest) <- Text.uncons v
  case Text.decimal rest of
    Right (n, "") -> pure n
    _ -> Nothing

mkVersion :: FromJSON v => Integer -> Maybe v
mkVersion n = case Aeson.fromJSON (Aeson.Number (fromIntegral n)) of
  Aeson.Error _ -> Nothing
  Aeson.Success v -> pure v

versionHeader :: CI.CI ByteString
versionHeader = "X-Wire-API-Version"

--------------------------------------------------------------------------------
-- Servant combinators

data Until v

data From v

instance
  ( SingI v,
    Ord (Demote (KindOf v)),
    Enum (Demote (KindOf v)),
    SingKind (KindOf v),
    HasServer api ctx
  ) =>
  HasServer (Until v :> api) ctx
  where
  type ServerT (Until v :> api) m = ServerT api m

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
                  (either (const 0) id . parseHeader)
                  (lookup versionHeader (Wai.requestHeaders req))
        when (v >= demote @v) $
          delayedFail err404

  hoistServerWithContext _ ctx f =
    hoistServerWithContext (Proxy @api) ctx f

instance HasSwagger api => HasSwagger (Until v :> api) where
  toSwagger _ = mempty

instance RoutesToPaths api => RoutesToPaths (Until v :> api) where
  getRoutes = getRoutes @api

instance
  ( SingI v,
    Ord (Demote (KindOf v)),
    Enum (Demote (KindOf v)),
    SingKind (KindOf v),
    HasServer api ctx
  ) =>
  HasServer (From v :> api) ctx
  where
  type ServerT (From v :> api) m = ServerT api m

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
                  (either (const 0) id . parseHeader)
                  (lookup versionHeader (Wai.requestHeaders req))
        when (v < demote @v) $
          delayedFail err404

  hoistServerWithContext _ ctx f =
    hoistServerWithContext (Proxy @api) ctx f

instance HasSwagger api => HasSwagger (From v :> api) where
  toSwagger _ = toSwagger (Proxy @api)

instance RoutesToPaths api => RoutesToPaths (From v :> api) where
  getRoutes = getRoutes @api
