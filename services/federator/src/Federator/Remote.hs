{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Federator.Remote
  ( Remote (..),
    RemoteError (..),
    interpretRemote,
    discoverAndCall,
    blessedCiphers,
  )
where

import Control.Lens ((^.))
import Data.Binary.Builder
import Data.ByteString.Conversion (toByteString')
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import Data.Domain
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.X509 as X509
import qualified Data.X509.Validation as X509
import Federator.Discovery
import Federator.Env (TLSSettings, caStore, creds)
import Federator.Error
import Federator.Validation
import Imports
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP2.Client as HTTP2
import Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Error
import Wire.Network.DNS.SRV

data RemoteError = RemoteError SrvTarget FederatorClientHTTP2Error
  deriving (Show)

instance AsWai RemoteError where
  toWai (RemoteError _ e) = federationRemoteHTTP2Error e

  waiErrorDescription (RemoteError tgt e) =
    "Error while connecting to " <> displayTarget tgt <> ": "
      <> Text.pack (displayException e)

displayTarget :: SrvTarget -> Text
displayTarget (SrvTarget hostname port) =
  Text.decodeUtf8With Text.lenientDecode hostname
    <> ":"
    <> Text.pack (show port)

data Remote m a where
  DiscoverAndCall ::
    Domain ->
    Component ->
    Text ->
    [HTTP.Header] ->
    Builder ->
    Remote m (HTTP.Status, Builder)

makeSem ''Remote

interpretRemote ::
  Members
    '[ Embed IO,
       DiscoverFederator,
       Error DiscoveryFailure,
       Error RemoteError,
       Input TLSSettings
     ]
    r =>
  Sem (Remote ': r) a ->
  Sem r a
interpretRemote = interpret $ \case
  DiscoverAndCall domain component rpc headers body -> do
    target@(SrvTarget hostname port) <- discoverFederatorWithError domain
    settings <- input
    let path =
          LBS.toStrict . toLazyByteString $
            HTTP.encodePathSegments ["federation", componentName component, rpc]
        req' = HTTP2.requestBuilder HTTP.methodPost path headers body
        tlsConfig = mkTLSConfig settings hostname port
    (status, _, result) <-
      mapError (RemoteError target) . (fromEither =<<) . embed $
        performHTTP2Request (Just tlsConfig) req' hostname (fromIntegral port)
    pure (status, result)

mkTLSConfig :: TLSSettings -> ByteString -> Word16 -> TLS.ClientParams
mkTLSConfig settings hostname port =
  ( defaultParamsClient
      (Text.unpack (Text.decodeUtf8With Text.lenientDecode hostname))
      (toByteString' port)
  )
    { TLS.clientSupported =
        def
          { TLS.supportedCiphers = blessedCiphers,
            -- FUTUREWORK: Figure out if we can drop TLS 1.2
            TLS.supportedVersions = [TLS.TLS12, TLS.TLS13]
          },
      TLS.clientHooks =
        def
          { TLS.onServerCertificate =
              X509.validate
                X509.HashSHA256
                X509.defaultHooks {X509.hookValidateName = validateDomainName}
                X509.defaultChecks {X509.checkLeafKeyPurpose = [X509.KeyUsagePurpose_ServerAuth]},
            TLS.onCertificateRequest = \_ -> pure (Just (settings ^. creds)),
            TLS.onSuggestALPN = pure (Just ["h2"]) -- we only support HTTP2
          },
      TLS.clientShared = def {TLS.sharedCAStore = settings ^. caStore}
    }

-- FUTUREWORK: get review on blessed ciphers
-- (https://wearezeta.atlassian.net/browse/SQCORE-910)
blessedCiphers :: [Cipher]
blessedCiphers =
  [ TLS.cipher_TLS13_AES128CCM8_SHA256,
    TLS.cipher_TLS13_AES128CCM_SHA256,
    TLS.cipher_TLS13_AES128GCM_SHA256,
    TLS.cipher_TLS13_AES256GCM_SHA384,
    TLS.cipher_TLS13_CHACHA20POLY1305_SHA256,
    -- For TLS 1.2 (copied from default nginx ingress config):
    TLS.cipher_ECDHE_ECDSA_AES256GCM_SHA384,
    TLS.cipher_ECDHE_RSA_AES256GCM_SHA384,
    TLS.cipher_ECDHE_RSA_AES128GCM_SHA256,
    TLS.cipher_ECDHE_ECDSA_AES128GCM_SHA256,
    TLS.cipher_ECDHE_ECDSA_CHACHA20POLY1305_SHA256,
    TLS.cipher_ECDHE_RSA_CHACHA20POLY1305_SHA256
  ]
