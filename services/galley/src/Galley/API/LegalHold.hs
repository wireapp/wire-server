module Galley.API.LegalHold where

import Imports
import Galley.API.Error
import Bilge.Retry
import Brig.Types.Provider
import Brig.Types.Team.LegalHold
import Control.Exception.Enclosed (handleAny)
import Control.Lens
import Control.Monad.Catch
import Control.Retry
import Data.ByteString.Conversion.To
import Data.Id
import Data.Misc
import Galley.API.Util
import Galley.App
import Galley.Types.Teams
import Network.HTTP.Types
import Network.HTTP.Types.Status (status201)
import Network.Wai
import Network.Wai.Predicate hiding (setStatus, result, or)
import Network.Wai.Utilities as Wai
import Ssl.Util

import qualified Bilge
import qualified Data.ByteString.Lazy.Char8   as LC8
import qualified Galley.Data                  as Data
import qualified Galley.Data.LegalHold        as LegalHoldData
import qualified Network.HTTP.Client          as Http
import qualified OpenSSL.EVP.Digest           as SSL
import qualified OpenSSL.EVP.PKey             as SSL
import qualified OpenSSL.PEM                  as SSL
import qualified OpenSSL.RSA                  as SSL
import qualified Ssl.Util                     as SSL


createSettings :: UserId ::: TeamId ::: JsonRequest NewLegalHoldService ::: JSON -> Galley Response
createSettings (zusr ::: tid ::: req ::: _) = do
    membs <- Data.teamMembers tid
    void $ permissionCheck zusr ChangeLegalHoldTeamSettings membs

    service :: NewLegalHoldService
        <- fromJsonBody req
    (_key :: ServiceKey, fpr :: Fingerprint Rsa)
        <- validateServiceKey (newLegalHoldServiceKey service)
               >>= maybe (throwM legalHoldServiceInvalidKey) pure
    checkLegalHoldServiceStatus fpr (newLegalHoldServiceUrl service)

    LegalHoldData.createSettings (legalHoldService service)
    pure $ responseLBS status201 [] mempty


-- | Get /status from legal hold service; throw 'Wai.Error' with 400 if things go wrong.
checkLegalHoldServiceStatus :: Fingerprint Rsa -> HttpsUrl -> Galley ()
checkLegalHoldServiceStatus fpr (HttpsUrl url) = do
    (mgr, verifyFingerprints) <- view (extEnv . extGetManager)
    let verified = verifyFingerprints [fpr]
    rs <- extHandleAll (const $ throwM legalHoldServiceUnavailable) $ do
        recovering x3 httpHandlers $ const $ liftIO $
            withVerifiedSslConnection verified mgr reqBuilder $ \req ->
                Http.httpLbs req mgr
    if | Bilge.statusCode rs < 400 -> pure ()
       | otherwise -> throwM legalHoldServiceBadResponse
  where
    reqBuilder :: Http.Request -> Http.Request
    reqBuilder
        = maybe id Bilge.host (Bilge.extHost url)
        . Bilge.port (fromMaybe 443 (Bilge.extPort url))
        . Bilge.paths ["status"]
        . Bilge.method GET
        . Bilge.secure
        . Bilge.expect2xx

    x3 :: RetryPolicy
    x3 = limitRetries 3 <> exponentialBackoff 100000

    extHandleAll :: MonadCatch m => (SomeException -> m a) -> m a -> m a
    extHandleAll f ma = catches ma
        [ Handler $ \(ex :: SomeAsyncException) -> throwM ex
        , Handler $ \(ex :: SomeException)      -> f ex
        ]

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
