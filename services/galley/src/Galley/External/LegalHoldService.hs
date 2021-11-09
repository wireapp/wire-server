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
  )
where

import qualified Bilge
import Bilge.Response
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Control.Exception.Enclosed (handleAny)
import Data.Aeson
import Data.ByteString.Conversion.To
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Id
import Data.Misc
import Galley.API.Error
import Galley.App
import Galley.Effects.LegalHoldStore as LegalHoldData
import Galley.External.LegalHoldService.Types
import Imports
import qualified Network.HTTP.Client as Http
import Network.HTTP.Types
import qualified OpenSSL.EVP.Digest as SSL
import qualified OpenSSL.EVP.PKey as SSL
import qualified OpenSSL.PEM as SSL
import qualified OpenSSL.RSA as SSL
import Polysemy
import Polysemy.Error
import qualified Ssl.Util as SSL
import qualified System.Logger.Class as Log

----------------------------------------------------------------------
-- api

-- | Get /status from legal hold service; throw 'Wai.Error' if things go wrong.
checkLegalHoldServiceStatus ::
  Members '[Error LegalHoldError, LegalHoldStore] r =>
  Fingerprint Rsa ->
  HttpsUrl ->
  Galley r ()
checkLegalHoldServiceStatus fpr url = do
  resp <- liftSem $ makeVerifiedRequestFreshManager fpr url reqBuilder
  if
      | Bilge.statusCode resp < 400 -> pure ()
      | otherwise -> do
        Log.info . Log.msg $ showResponse resp
        liftSem $ throw LegalHoldServiceBadResponse
  where
    reqBuilder :: Http.Request -> Http.Request
    reqBuilder =
      Bilge.paths ["status"]
        . Bilge.method GET
        . Bilge.expect2xx

-- | @POST /initiate@.
requestNewDevice ::
  Members '[Error LegalHoldError, LegalHoldStore] r =>
  TeamId ->
  UserId ->
  Galley r NewLegalHoldClient
requestNewDevice tid uid = do
  resp <- makeLegalHoldServiceRequest tid reqParams
  case eitherDecode (responseBody resp) of
    Left e -> do
      Log.info . Log.msg $ "Error decoding NewLegalHoldClient: " <> e
      liftSem $ throw LegalHoldServiceBadResponse
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
  Members '[Error LegalHoldError, LegalHoldStore] r =>
  ClientId ->
  TeamId ->
  UserId ->
  -- | TODO: Replace with 'LegalHold' token type
  OpaqueAuthToken ->
  Galley r ()
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
  Members '[Error LegalHoldError, LegalHoldStore] r =>
  TeamId ->
  UserId ->
  Galley r ()
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
makeLegalHoldServiceRequest ::
  Members '[Error LegalHoldError, LegalHoldStore] r =>
  TeamId ->
  (Http.Request -> Http.Request) ->
  Galley r (Http.Response LC8.ByteString)
makeLegalHoldServiceRequest tid reqBuilder = liftSem $ do
  maybeLHSettings <- LegalHoldData.getSettings tid
  lhSettings <- case maybeLHSettings of
    Nothing -> throw LegalHoldServiceNotRegistered
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
