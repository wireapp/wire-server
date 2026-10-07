{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.WebPushStore where

import Control.Lens ((^.))
import Data.Id
import Data.UUID
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.Push.V2.WebSubscription

-- | A web push subscription resolved at dispatch time. The dispatcher fetches
-- these for a user, then POSTs an RFC 8030 message to each 'wpaEndpoint',
-- encrypting for the recipient's 'wpaKeys'. Analogous to the native
-- 'Gundeck.Push.Native.Types.Address'.
data WebPushAddress = WebPushAddress
  { wpaUser :: !UserId,
    wpaConn :: !ConnId,
    wpaClient :: !ClientId,
    wpaEndpoint :: !EndpointUrl,
    wpaKeys :: !WebPushKeys
  }
  deriving stock (Eq)

-- | Custom 'Show' that omits 'wpaKeys', mirroring the native
-- 'Gundeck.Push.Native.Types.Address' Show instance. 'wpaKeys' carries the RFC
-- 8291 auth secret which must not leak into logs or error contexts.
instance Show WebPushAddress where
  show a =
    showString "WebPushAddress"
      . showString "{ user = "
      . shows a.wpaUser
      . showString ", conn = "
      . shows a.wpaConn
      . showString ", client = "
      . shows a.wpaClient
      . showString ", endpoint = "
      . shows a.wpaEndpoint
      . showString ", keys = <redacted>"
      $ "}"

-- The 'PostgresMarshall' instances live here (next to the type they marshal),
-- matching the convention used by 'Wire.AppStore.StoredApp'. Keeping them here
-- avoids orphan instances.
instance
  PostgresMarshall
    (UUID, Text, Text, Text, Text, ByteString)
    WebPushAddress
  where
  postgresMarshall a =
    ( postgresMarshall a.wpaUser,
      postgresMarshall a.wpaClient,
      postgresMarshall a.wpaEndpoint,
      postgresMarshall (a.wpaKeys ^. wpkP256dh),
      postgresMarshall (a.wpaKeys ^. wpkAuth),
      postgresMarshall a.wpaConn
    )

instance
  PostgresUnmarshall
    (UUID, Text, Text, Text, Text, ByteString)
    WebPushAddress
  where
  postgresUnmarshall (uid, client, endpoint, p256dh, auth, conn) = do
    u <- postgresUnmarshall @UUID @UserId uid
    c <- postgresUnmarshall @Text @ClientId client
    ep <- postgresUnmarshall @Text @EndpointUrl endpoint
    p <- postgresUnmarshall @Text @P256dhKey p256dh
    a <- postgresUnmarshall @Text @AuthSecret auth
    conn' <- postgresUnmarshall @ByteString @ConnId conn
    pure $ WebPushAddress u conn' c ep (WebPushKeys p a)

-- | Persistence effect for web push subscriptions. Stores browser-supplied
-- push-service endpoints so gundeck can deliver notifications via the W3C Push
-- API (acting as the RFC 8030 application server).
--
-- This is a library concern in @wire-subsystems@ (Postgres-backed), decoupled
-- from gundeck's legacy Cassandra stack.
data WebPushStore m a where
  -- | Register or refresh a subscription. Upserting on
  -- @(user, client, endpoint)@ so re-registration updates keys / expiry.
  InsertSubscription :: UserId -> WebPushSubscription -> ConnId -> WebPushStore m ()
  -- | Fetch all subscriptions for a user (the per-user dispatch hot path).
  LookupSubscriptions :: UserId -> WebPushStore m [WebPushAddress]
  -- | Remove a single subscription identified by its endpoint.
  DeleteSubscription :: UserId -> EndpointUrl -> WebPushStore m ()
  -- | Remove every subscription for a user (GDPR / account deletion).
  DeleteAllForUser :: UserId -> WebPushStore m ()
  -- | Drop subscriptions whose 'wpsExpirationTime' has passed. Called
  -- periodically by the dispatcher to keep the table from accumulating stale
  -- rows that the push service would only reject anyway.
  PurgeExpired :: UserId -> WebPushStore m ()

makeSem ''WebPushStore
