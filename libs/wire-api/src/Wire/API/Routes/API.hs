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

module Wire.API.Routes.API
  ( ServiceAPI (..),
    API,
    hoistAPIHandler,
    hoistAPI,
    mkAPI,
    mkNamedAPI,
    (<@>),
    ServerEffect (..),
    ServerEffects (..),
    hoistServerWithDomain,
  )
where

import Data.Domain
import Data.Kind
import Data.OpenApi qualified as S
import Data.Proxy
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import Servant hiding (Union)
import Servant.OpenApi
import Wire.API.Error
import Wire.API.Routes.Named
import Wire.API.Routes.Version

class ServiceAPI service (v :: Version) where
  type ServiceAPIRoutes service
  type SpecialisedAPIRoutes v service :: Type
  type SpecialisedAPIRoutes v service = SpecialiseToVersion v (ServiceAPIRoutes service)
  serviceSwagger :: (HasOpenApi (SpecialisedAPIRoutes v service)) => S.OpenApi
  serviceSwagger = toOpenApi (Proxy @(SpecialisedAPIRoutes v service))

instance ServiceAPI VersionAPITag v where
  type ServiceAPIRoutes VersionAPITag = VersionAPI

-- | A Servant handler on a polysemy stack. This is used to help with type inference.
newtype API api r = API {unAPI :: ServerT api (Sem r)}

-- | Convert a polysemy handler to an 'API' value.
mkAPI ::
  forall r0 api.
  (HasServer api '[Domain], ServerEffects (DeclaredErrorEffects api) r0) =>
  ServerT api (Sem (Append (DeclaredErrorEffects api) r0)) ->
  API api r0
mkAPI h = API $ hoistServerWithDomain @api (interpretServerEffects @(DeclaredErrorEffects api) @r0) h

-- | Convert a polysemy handler to a named 'API' value.
mkNamedAPI ::
  forall name r0 api.
  (HasServer api '[Domain], ServerEffects (DeclaredErrorEffects api) r0) =>
  ServerT api (Sem (Append (DeclaredErrorEffects api) r0)) ->
  API (Named name api) r0
mkNamedAPI = API . Named . unAPI . mkAPI @r0 @api

-- | Combine APIs.
(<@>) :: API api1 r -> API api2 r -> API (api1 :<|> api2) r
(<@>) (API h1) (API h2) = API (h1 :<|> h2)

infixr 3 <@>

-- Servant needs a context type argument here that contains *at least* the
-- context types required by all the HasServer instances. In reality, this should
-- not be necessary, because the contexts are only used by the @route@ functions,
-- but unfortunately the 'hoistServerWithContext' function is also part of the
-- 'HasServer' typeclass, even though it cannot possibly make use of its @context@
-- type argument.
hoistServerWithDomain ::
  forall api m n.
  (HasServer api '[Domain]) =>
  (forall x. m x -> n x) ->
  ServerT api m ->
  ServerT api n
hoistServerWithDomain = hoistServerWithContext (Proxy @api) (Proxy @'[Domain])

hoistAPIHandler ::
  forall api r n.
  (HasServer api '[Domain]) =>
  (forall x. Sem r x -> n x) ->
  API api r ->
  ServerT api n
hoistAPIHandler f = hoistServerWithDomain @api f . unAPI

hoistAPI ::
  forall api1 api2 r1 r2.
  (ServerT api1 (Sem r1) -> ServerT api2 (Sem r2)) ->
  API api1 r1 ->
  API api2 r2
hoistAPI f = API . f . unAPI

class ServerEffect eff r where
  interpretServerEffect :: Sem (eff ': r) a -> Sem r a

class ServerEffects r r1 where
  interpretServerEffects :: Sem (Append r r1) a -> Sem r1 a

instance ServerEffects '[] r where
  interpretServerEffects = id

instance (ServerEffects r r1, ServerEffect eff (Append r r1)) => ServerEffects (eff ': r) r1 where
  interpretServerEffects = interpretServerEffects @r @r1 . interpretServerEffect @eff @(Append r r1)

instance (KnownError (MapError e), Member (Error DynError) r) => ServerEffect (ErrorS e) r where
  interpretServerEffect = mapToDynamicError

instance (KnownError (MapError e), Member (Error DynError) r) => ServerEffect (Error (Tagged e Text)) r where
  interpretServerEffect = mapError $ \msg -> (dynError @(MapError e)) {eMessage = unTagged msg}
