{-# LANGUAGE RecordWildCards #-}
-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module Wire.API.Federation.API
  ( FedApi,
    HasFedEndpoint,
    HasUnsafeFedEndpoint,
    FederationMonad (..),
    IsNamed (..),
    nameVal,
    fedClient,
    fedQueueClient,
    sendBundle,
    fedClientIn,
    module X,

    -- * Re-exports
    Component (..),
    makeConversationUpdateBundle,
  )
where

import Data.Aeson
import Data.Domain
import Data.Kind
import Data.Proxy
import Data.Singletons
import Data.Text qualified as Text
import GHC.TypeLits
import Imports
import Pulsar qualified as P
import Servant.Client
import Servant.Client.Core
import Wire.API.Component as X
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Cargohold
import Wire.API.Federation.API.Galley
import Wire.API.Federation.API.Util
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Endpoint
import Wire.API.Federation.HasNotificationEndpoint
import Wire.API.Federation.Version
import Wire.API.Routes.Named

-- Note: this type family being injective means that in most cases there is no need
-- to add component annotations when invoking the federator client
type family FedApi (comp :: Component) = (api :: Type) | api -> comp

type instance FedApi 'Galley = GalleyApi

type instance FedApi 'Brig = BrigApi

type instance FedApi 'Cargohold = CargoholdApi

type HasFedEndpoint comp api name = (HasUnsafeFedEndpoint comp api name)

-- | Like 'HasFedEndpoint', but doesn't propagate a 'CallsFed' constraint.
-- Useful for tests, but unsafe in the sense that incorrect usage will allow
-- you to forget about some federated calls.
type HasUnsafeFedEndpoint comp api name = 'Just api ~ LookupEndpoint (FedApi comp) name

nameVal :: forall {k} (name :: k). (IsNamed name) => Text
nameVal = nameVal' @k @name

class IsNamed (name :: k) where
  nameVal' :: Text

instance (KnownSymbol name) => IsNamed (name :: Symbol) where
  nameVal' = Text.pack (symbolVal (Proxy @name))

instance (IsNamed name, SingI v) => IsNamed (Versioned (v :: Version) name) where
  nameVal' = versionText (demote @v) <> "-" <> nameVal @name

class FederationMonad (fedM :: Component -> Type -> Type) where
  fedClientWithProxy ::
    forall (comp :: Component) name api.
    ( HasClient (fedM comp) api,
      HasFedEndpoint comp api name,
      KnownComponent comp,
      IsNamed name,
      Typeable (Client (fedM comp) api)
    ) =>
    Proxy name ->
    Proxy api ->
    Proxy (fedM comp) ->
    Client (fedM comp) api

instance FederationMonad FederatorClient where
  fedClientWithProxy _ = clientIn

-- | Return a client for a named endpoint.
fedClient ::
  forall (comp :: Component) name fedM (showcomp :: Symbol) api.
  ( showcomp ~ ShowComponent comp,
    HasFedEndpoint comp api name,
    HasClient (fedM comp) api,
    KnownComponent comp,
    IsNamed name,
    FederationMonad fedM,
    Typeable (Client (fedM comp) api)
  ) =>
  Client (fedM comp) api
fedClient = fedClientWithProxy (Proxy @name) (Proxy @api) (Proxy @(fedM comp))

fedClientIn ::
  forall (comp :: Component) (name :: Symbol) m api.
  (HasFedEndpoint comp api name, HasClient m api) =>
  Client m api
fedClientIn = clientIn (Proxy @api) (Proxy @m)

sendBundle ::
  (KnownComponent c) =>
  PayloadBundle c ->
  FedQueueClient c ()
sendBundle bundle = do
  env <- ask
  liftIO $
    -- TODO: Do we still need to ensure subsriptions?
    -- ensureQueue env.connection env.targetDomain._domainText
    -- void $ publishMsg env.connection exchange (routingKey env.targetDomain._domainText) msg
    let topic =
          P.Topic
            { P.type' = env.deliveryMode,
              P.tenant = P.Tenant "default",
              P.namespace = P.NameSpace "default",
              P.name = P.TopicName (routingKey env.targetDomain._domainText)
            }
     in P.runPulsar env.connection $ do
          P.Producer {..} <- P.newProducer topic
          send . P.PulsarMessage . encode $ bundle

fedQueueClient ::
  forall {k} (tag :: k) c.
  ( HasNotificationEndpoint tag,
    HasVersionRange tag,
    HasFedPath tag,
    KnownComponent (NotificationComponent k),
    ToJSON (Payload tag),
    c ~ NotificationComponent k
  ) =>
  Payload tag ->
  FedQueueClient c ()
fedQueueClient payload = sendBundle =<< makeBundle @tag payload
