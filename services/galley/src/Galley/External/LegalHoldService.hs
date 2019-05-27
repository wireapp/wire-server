module Galley.External.LegalHoldService
    ( -- * api
      checkLegalHoldServiceStatus
    , requestNewDevice

      -- * helpers
    , validateServiceKey
    ) where

import Imports
import Data.Id
import Galley.API.Error
import Bilge.Retry
import Bilge.Response
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Control.Exception.Enclosed (handleAny)
import Control.Lens hiding ((.=), (#))
import Control.Monad.Catch
import Control.Retry
import Data.Aeson
import Data.ByteString.Conversion.To
import Data.Misc
import Galley.App
import Network.HTTP.Types
import Ssl.Util

import qualified Bilge
import qualified Galley.Data.LegalHold        as LegalHoldData
import qualified Data.ByteString.Lazy.Char8   as LC8
import qualified Network.HTTP.Client          as Http
import qualified OpenSSL.EVP.Digest           as SSL
import qualified OpenSSL.EVP.PKey             as SSL
import qualified OpenSSL.PEM                  as SSL
import qualified OpenSSL.RSA                  as SSL
import qualified Ssl.Util                     as SSL


----------------------------------------------------------------------
-- api

-- | Get /status from legal hold service; throw 'Wai.Error' if things go wrong.
checkLegalHoldServiceStatus :: Fingerprint Rsa -> HttpsUrl -> Galley ()
checkLegalHoldServiceStatus fpr url = do
    rs <- makeVerifiedRequest fpr url reqBuilder
    if | Bilge.statusCode rs < 400 -> pure ()
       | otherwise -> throwM legalHoldServiceBadResponse
  where
    reqBuilder :: Http.Request -> Http.Request
    reqBuilder
        = Bilge.paths ["status"]
        . Bilge.method GET
        . Bilge.expect2xx

-- | @POST /initiate@.
requestNewDevice :: TeamId -> UserId -> Galley NewLegalHoldClient
requestNewDevice tid uid = do
    resp <- makeLegalHoldServiceRequest tid reqParams
    case decode (responseBody resp) of
        Nothing -> throwM legalHoldServiceBadResponse
        Just client -> pure client
  where
    reqParams =
        Bilge.paths ["initiate"]
      . Bilge.json (RequestNewLegalHoldClient uid tid)
      . Bilge.method POST
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
        Nothing -> throwM legalHoldServiceUnavailable
        Just lhSettings -> pure lhSettings

    let LegalHoldService
         { legalHoldServiceUrl = baseUrl
         , legalHoldServiceFingerprint = fpr
         , legalHoldServiceToken = serviceToken
         } = lhSettings
    makeVerifiedRequest fpr baseUrl $ mkReqBuilder serviceToken
  where
    mkReqBuilder token =
        reqBuilder
        -- TODO: Verify this is correct:
        . Bilge.header "Authentication" (toByteString' token)

-- | Check that the given fingerprint is valid and make the request over ssl.
-- If the team has a device registered use 'makeLegalHoldServiceRequest' instead.
makeVerifiedRequest :: Fingerprint Rsa -> HttpsUrl -> (Http.Request -> Http.Request) -> Galley (Http.Response LC8.ByteString)
makeVerifiedRequest fpr (HttpsUrl url) reqBuilder = do
    (mgr, verifyFingerprints) <- view (extEnv . extGetManager)
    let verified = verifyFingerprints [fpr]
    extHandleAll (const $ throwM legalHoldServiceUnavailable) $ do
        recovering x3 httpHandlers $ const $ liftIO $
            withVerifiedSslConnection verified mgr (reqBuilderMods . reqBuilder) $ \req ->
                Http.httpLbs req mgr
  where
    reqBuilderMods =
        maybe id Bilge.host (Bilge.extHost url)
        . Bilge.port (fromMaybe 443 (Bilge.extPort url))
        . Bilge.secure

    x3 :: RetryPolicy
    x3 = limitRetries 3 <> exponentialBackoff 100000

    extHandleAll :: MonadCatch m => (SomeException -> m a) -> m a -> m a
    extHandleAll f ma = catches ma
        [ Handler $ \(ex :: SomeAsyncException) -> throwM ex
        , Handler $ \(ex :: SomeException)      -> f ex
        ]

-- | Copied unchanged from "Brig.Provider.API".  Interpret a service certificate and extract
-- key and fingerprint.  (This only has to be in 'MonadIO' because the FFI in OpenSSL works
-- like that.)
--
-- FUTUREWORK: It would be nice to move (part of) this to ssl-util, but it has types from
-- brig-types and types-common.
validateServiceKey :: MonadIO m => ServiceKeyPEM -> m (Maybe (ServiceKey, Fingerprint Rsa))
validateServiceKey pem = liftIO $ readPublicKey >>= \pk ->
    case join (SSL.toPublicKey <$> pk) of
        Nothing  -> return Nothing
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
    readPublicKey = handleAny
        (const $ return Nothing)
        (SSL.readPublicKey (LC8.unpack (toByteString pem)) >>= return . Just)

    minRsaKeySize :: Int
    minRsaKeySize = 256 -- Bytes (= 2048 bits)
