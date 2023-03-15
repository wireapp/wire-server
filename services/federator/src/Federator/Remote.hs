{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Federator.Remote
  ( Remote (..),
    RemoteError (..),
    interpretRemote,
    discoverAndCall,
    blessedCiphers,
  )
where

import qualified Control.Exception as E
import Control.Lens ((^.))
import Control.Monad.Codensity
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
import Servant.Client.Core
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Error
import Wire.Network.DNS.SRV

-- | An error that can occur as a result of making a request to a remote
-- federator.
data RemoteError
  = -- | This means that an error occurred while trying to make a request to a
    -- remote federator.
    RemoteError SrvTarget FederatorClientHTTP2Error
  | -- | This means that a request to a remote federator returned an error
    -- response. The error response could be due to an error in the remote
    -- federator itself, or in the services it proxied to.
    RemoteErrorResponse SrvTarget HTTP.Status LByteString
  deriving (Show)

instance AsWai RemoteError where
  toWai (RemoteError _ e) = federationRemoteHTTP2Error e
  toWai (RemoteErrorResponse _ status _) =
    federationRemoteResponseError status

  waiErrorDescription (RemoteError tgt e) =
    "Error while connecting to "
      <> displayTarget tgt
      <> ": "
      <> Text.pack (displayException e)
  waiErrorDescription (RemoteErrorResponse tgt status body) =
    "Federator at "
      <> displayTarget tgt
      <> " failed with status code "
      <> Text.pack (show (HTTP.statusCode status))
      <> ": "
      <> Text.decodeUtf8With Text.lenientDecode (LBS.toStrict body)

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
    Remote m StreamingResponse

makeSem ''Remote

interpretRemote ::
  ( Member (Embed (Codensity IO)) r,
    Member DiscoverFederator r,
    Member (Error DiscoveryFailure) r,
    Member (Error RemoteError) r,
    Member (Input TLSSettings) r
  ) =>
  Sem (Remote ': r) a ->
  Sem r a
interpretRemote = interpret $ \case
  DiscoverAndCall domain component rpc headers body -> do
    target@(SrvTarget hostname port) <- discoverFederatorWithError domain
    settings <- input
    let path =
          LBS.toStrict . toLazyByteString $
            HTTP.encodePathSegments ["federation", componentName component, rpc]
        -- filter out Host header, because the HTTP2 client adds it back
        headers' = filter ((/= "Host") . fst) headers
        req' = HTTP2.requestBuilder HTTP.methodPost path headers' body
        tlsConfig = mkTLSConfig settings hostname port

    resp <- mapError (RemoteError target) . (fromEither @FederatorClientHTTP2Error =<<) . embed $
      Codensity $ \k ->
        E.catch
          (withHTTP2Request (Just tlsConfig) req' hostname (fromIntegral port) (k . Right))
          (k . Left)

    unless (HTTP.statusIsSuccessful (responseStatusCode resp)) $ do
      bdy <- embed @(Codensity IO) . liftIO $ streamingResponseStrictBody resp
      throw $
        RemoteErrorResponse
          target
          (responseStatusCode resp)
          (toLazyByteString bdy)
    pure resp

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

-- Context and possible future work see
-- https://wearezeta.atlassian.net/browse/FS-33
-- https://wearezeta.atlassian.net/browse/FS-444
-- https://wearezeta.atlassian.net/browse/FS-443
--
-- The current list is compliant to TR-02102-2
-- https://www.bsi.bund.de/SharedDocs/Downloads/EN/BSI/Publications/TechGuidelines/TG02102/BSI-TR-02102-2.html
blessedCiphers :: [Cipher]
blessedCiphers =
  [ TLS.cipher_TLS13_AES128CCM8_SHA256,
    TLS.cipher_TLS13_AES128CCM_SHA256,
    TLS.cipher_TLS13_AES128GCM_SHA256,
    TLS.cipher_TLS13_AES256GCM_SHA384,
    -- For TLS 1.2 (copied from default nginx ingress config):
    TLS.cipher_ECDHE_ECDSA_AES256GCM_SHA384,
    TLS.cipher_ECDHE_RSA_AES256GCM_SHA384
  ]
