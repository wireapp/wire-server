module Galley.External.LegalHoldService
    ( checkLegalHoldServiceStatus
    , validateServiceKey
    , requestNewDevice
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

-- | Get /status from legal hold service; throw 'Wai.Error' with 400 if things go wrong.
checkLegalHoldServiceStatus :: Fingerprint Rsa -> HttpsUrl -> Galley ()
checkLegalHoldServiceStatus fpr (HttpsUrl url) = do
    rs <- makeVerifiedRequest fpr reqBuilder
    if | Bilge.statusCode rs < 400 -> pure ()
       | otherwise -> throwM legalHoldServiceBadResponse
  where
    reqBuilder :: Http.Request -> Http.Request
    reqBuilder
        = maybe id Bilge.host (Bilge.extHost url)
        . Bilge.paths ["status"]
        . Bilge.method GET
        . Bilge.secure
        . Bilge.expect2xx

-- | Copied unchanged from "Brig.Provider.API".  It would be nice to move (part of) this to
-- ssl-util, but it has types from brig-types and types-common.
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

-- | Lookup LH service settings for a team and make a verified request
makeLHServiceRequest :: TeamId -> (Http.Request -> Http.Request) -> Galley (Http.Response LC8.ByteString)
makeLHServiceRequest tid reqBuilder = do
    maybeLHSettings <- LegalHoldData.getSettings tid
    lhSettings <- case maybeLHSettings of
        Nothing -> throwM legalHoldServiceUnavailable
        Just lhSettings -> pure lhSettings

    let LegalHoldService
         { legalHoldServiceUrl = baseUrl
         , legalHoldServiceFingerprint = fpr
         , legalHoldServiceToken = serviceToken
         } = lhSettings
    makeVerifiedRequest fpr $ mkReqBuilder baseUrl serviceToken
  where
    mkReqBuilder (HttpsUrl url) token =
        reqBuilder
        . Bilge.port (fromMaybe 443 (Bilge.extPort url))
        . Bilge.secure
        -- TODO: Verify this is correct:
        . Bilge.header "Authentication" (toByteString' token)

-- | Check that the given fingerprint is valid and make the request over ssl.
-- If the team has a device registered use 'makeLHServiceRequest' instead.
makeVerifiedRequest :: Fingerprint Rsa -> (Http.Request -> Http.Request) -> Galley (Http.Response LC8.ByteString)
makeVerifiedRequest fpr reqBuilder = do
    (mgr, verifyFingerprints) <- view (extEnv . extGetManager)
    let verified = verifyFingerprints [fpr]
    extHandleAll (const $ throwM legalHoldServiceUnavailable) $ do
        recovering x3 httpHandlers $ const $ liftIO $
            withVerifiedSslConnection verified mgr reqBuilder $ \req ->
                Http.httpLbs req mgr
  where
    x3 :: RetryPolicy
    x3 = limitRetries 3 <> exponentialBackoff 100000

    extHandleAll :: MonadCatch m => (SomeException -> m a) -> m a -> m a
    extHandleAll f ma = catches ma
        [ Handler $ \(ex :: SomeAsyncException) -> throwM ex
        , Handler $ \(ex :: SomeException)      -> f ex
        ]

requestNewDevice :: TeamId -> UserId -> Galley NewLegalHoldClient
requestNewDevice tid uid = do
    resp <- makeLHServiceRequest tid reqParams
    case decode (responseBody resp) of
        Nothing -> throwM legalHoldServiceBadResponse
        Just client -> pure client
  where
    reqParams =
        Bilge.paths ["initiate"]
      . Bilge.json (InitiateRequest uid tid)
      . Bilge.method POST
      . Bilge.expect2xx
