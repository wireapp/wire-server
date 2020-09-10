{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Galley.External.LegalHoldService
  ( -- * api
    checkLegalHoldServiceStatus,
    requestNewDevice,
    confirmLegalHold,
    removeLegalHold,

    -- * helpers
    validateServiceKey,

    -- * types
    OpaqueAuthToken (..),
  )
where

import qualified Bilge
import Bilge.Response
import Bilge.Retry
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Control.Exception.Enclosed (handleAny)
import Control.Lens hiding ((#), (.=))
import Control.Monad.Catch
import Control.Retry
import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Conversion.To
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Id
import Data.Misc
import Galley.API.Error
import Galley.App
import qualified Galley.Data.LegalHold as LegalHoldData
import Imports
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types
import qualified OpenSSL.EVP.Digest as SSL
import qualified OpenSSL.EVP.PKey as SSL
import qualified OpenSSL.PEM as SSL
import qualified OpenSSL.RSA as SSL
import qualified OpenSSL.Session as SSL
import Ssl.Util
import qualified Ssl.Util as SSL
import qualified System.Logger.Class as Log
import URI.ByteString (uriPath)

----------------------------------------------------------------------
-- api

-- | Get /status from legal hold service; throw 'Wai.Error' if things go wrong.
checkLegalHoldServiceStatus :: Fingerprint Rsa -> HttpsUrl -> Galley ()
checkLegalHoldServiceStatus fpr url = do
  resp <- makeVerifiedRequestFreshManager fpr url reqBuilder
  if
      | Bilge.statusCode resp < 400 -> pure ()
      | otherwise -> do
        Log.info . Log.msg $ showResponse resp
        throwM legalHoldServiceBadResponse
  where
    reqBuilder :: Http.Request -> Http.Request
    reqBuilder =
      Bilge.paths ["status"]
        . Bilge.method GET
        . Bilge.expect2xx

-- | @POST /initiate@.
requestNewDevice :: TeamId -> UserId -> Galley NewLegalHoldClient
requestNewDevice tid uid = do
  resp <- makeLegalHoldServiceRequest tid reqParams
  case eitherDecode (responseBody resp) of
    Left e -> do
      Log.info . Log.msg $ "Error decoding NewLegalHoldClient: " <> e
      throwM legalHoldServiceBadResponse
    Right client -> pure client
  where
    reqParams =
      Bilge.paths ["initiate"]
        . Bilge.json (RequestNewLegalHoldClient uid tid)
        . Bilge.method POST
        . Bilge.acceptJson
        . Bilge.expect2xx

-- | @POST /confirm@
-- Confirm that a device has been linked to a user and provide an authorization token
confirmLegalHold ::
  ClientId ->
  TeamId ->
  UserId ->
  -- | TODO: Replace with 'LegalHold' token type
  OpaqueAuthToken ->
  Galley ()
confirmLegalHold clientId tid uid legalHoldAuthToken = do
  void $ makeLegalHoldServiceRequest tid reqParams
  where
    reqParams =
      Bilge.paths ["confirm"]
        . Bilge.json (LegalHoldServiceConfirm clientId uid tid (opaqueAuthTokenToText legalHoldAuthToken))
        . Bilge.method POST
        . Bilge.acceptJson
        . Bilge.expect2xx

-- | @POST /remove@
-- Inform the LegalHold Service that a user's legalhold has been disabled.
removeLegalHold ::
  TeamId ->
  UserId ->
  Galley ()
removeLegalHold tid uid = do
  void $ makeLegalHoldServiceRequest tid reqParams
  where
    reqParams =
      Bilge.paths ["remove"]
        . Bilge.json (LegalHoldServiceRemove uid tid)
        . Bilge.method POST
        . Bilge.acceptJson
        . Bilge.expect2xx

----------------------------------------------------------------------
-- helpers

-- | Lookup legal hold service settings for a team and make a request to the service.  Pins
-- the TSL fingerprint via 'makeVerifiedRequest' and passes the token so the service can
-- authenticate the request.
makeLegalHoldServiceRequest :: TeamId -> (Http.Request -> Http.Request) -> Galley (Http.Response LC8.ByteString)
makeLegalHoldServiceRequest tid reqBuilder = do
  maybeLHSettings <- LegalHoldData.getSettings tid
  lhSettings <- case maybeLHSettings of
    Nothing -> throwM legalHoldServiceNotRegistered
    Just lhSettings -> pure lhSettings
  let LegalHoldService
        { legalHoldServiceUrl = baseUrl,
          legalHoldServiceFingerprint = fpr,
          legalHoldServiceToken = serviceToken
        } = lhSettings
  makeVerifiedRequest fpr baseUrl $ mkReqBuilder serviceToken
  where
    mkReqBuilder token =
      reqBuilder
        . Bilge.header "Authorization" ("Bearer " <> toByteString' token)

makeVerifiedRequest :: Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> Galley (Http.Response LC8.ByteString)
makeVerifiedRequest fpr url reqBuilder = do
  (mgr, verifyFingerprints) <- view (extEnv . extGetManager)
  makeVerifiedRequestWithManager mgr verifyFingerprints fpr url reqBuilder

-- | NOTE: Use this function wisely - this creates a new manager _every_ time it is called.
--   We should really _only_ use it in `checkLegalHoldServiceStatus` for the time being because
--   this is where we check for signatures, etc. If we reuse the manager, we are likely to reuse
--   an existing connection which will _not_ cause the new public key to be verified.
makeVerifiedRequestFreshManager :: Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> Galley (Http.Response LC8.ByteString)
makeVerifiedRequestFreshManager fpr url reqBuilder = do
  ExtEnv (mgr, verifyFingerprints) <- liftIO initExtEnv
  makeVerifiedRequestWithManager mgr verifyFingerprints fpr url reqBuilder

-- | Check that the given fingerprint is valid and make the request over ssl.
-- If the team has a device registered use 'makeLegalHoldServiceRequest' instead.
makeVerifiedRequestWithManager :: Http.Manager -> ([Fingerprint Rsa] -> SSL.SSL -> IO ()) -> Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> Galley (Http.Response LC8.ByteString)
makeVerifiedRequestWithManager mgr verifyFingerprints fpr (HttpsUrl url) reqBuilder = do
  let verified = verifyFingerprints [fpr]
  extHandleAll errHandler $ do
    recovering x3 httpHandlers $
      const $
        liftIO $
          withVerifiedSslConnection verified mgr (reqBuilderMods . reqBuilder) $
            \req ->
              Http.httpLbs req mgr
  where
    reqBuilderMods =
      maybe id Bilge.host (Bilge.extHost url)
        . Bilge.port (fromMaybe 443 (Bilge.extPort url))
        . Bilge.secure
        . prependPath (uriPath url)
    errHandler e = do
      Log.info . Log.msg $ "error making request to legalhold service: " <> show e
      throwM legalHoldServiceUnavailable
    prependPath :: ByteString -> Http.Request -> Http.Request
    prependPath pth req = req {Http.path = pth </> Http.path req}
    -- append two paths with exactly one slash
    (</>) :: ByteString -> ByteString -> ByteString
    a </> b = fromMaybe a (BS.stripSuffix "/" a) <> "/" <> fromMaybe b (BS.stripPrefix "/" b)
    x3 :: RetryPolicy
    x3 = limitRetries 3 <> exponentialBackoff 100000
    extHandleAll :: MonadCatch m => (SomeException -> m a) -> m a -> m a
    extHandleAll f ma =
      catches
        ma
        [ Handler $ \(ex :: SomeAsyncException) -> throwM ex,
          Handler $ \(ex :: SomeException) -> f ex
        ]

-- | Copied unchanged from "Brig.Provider.API".  Interpret a service certificate and extract
-- key and fingerprint.  (This only has to be in 'MonadIO' because the FFI in OpenSSL works
-- like that.)
--
-- FUTUREWORK: It would be nice to move (part of) this to ssl-util, but it has types from
-- brig-types and types-common.
validateServiceKey :: MonadIO m => ServiceKeyPEM -> m (Maybe (ServiceKey, Fingerprint Rsa))
validateServiceKey pem =
  liftIO $
    readPublicKey >>= \pk ->
      case join (SSL.toPublicKey <$> pk) of
        Nothing -> return Nothing
        Just pk' -> do
          Just sha <- SSL.getDigestByName "SHA256"
          let size = SSL.rsaSize (pk' :: SSL.RSAPubKey)
          if size < minRsaKeySize
            then return Nothing
            else do
              fpr <- Fingerprint <$> SSL.rsaFingerprint sha pk'
              let bits = fromIntegral size * 8
              let key = ServiceKey RsaServiceKey bits pem
              return $ Just (key, fpr)
  where
    readPublicKey =
      handleAny
        (const $ return Nothing)
        (SSL.readPublicKey (LC8.unpack (toByteString pem)) >>= return . Just)
    minRsaKeySize :: Int
    minRsaKeySize = 256 -- Bytes (= 2048 bits)

-- Types

-- | When receiving tokens from other services which are 'just passing through'
-- it's error-prone useless extra work to parse and render them from JSON over and over again.
-- We'll just wrap them with this to give some level of typesafety and a reasonable JSON
-- instance
newtype OpaqueAuthToken = OpaqueAuthToken
  { opaqueAuthTokenToText :: Text
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON, ToByteString)
