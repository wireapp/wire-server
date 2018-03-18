{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}

module CargoHold.CloudFront
    ( CloudFront
    , Domain (..)
    , KeyPairId (..)
    , initCloudFront
    , signedURL
    ) where

import Control.AutoUpdate
import Control.Monad.Catch
import Control.Monad.IO.Class
import Crypto.Hash.Algorithms (SHA1 (..))
import Crypto.PubKey.RSA
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Conversion
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX
import Data.X509 (PrivKey (..))
import Data.X509.File
import Data.Yaml (FromJSON)
import GHC.Generics
import URI.ByteString

import qualified CargoHold.Error          as AWS
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Char8    as C8

newtype KeyPairId = KeyPairId Text
    deriving (Eq, Show, ToByteString, Generic, FromJSON)

newtype Domain = Domain Text
    deriving (Eq, Show, ToByteString, Generic, FromJSON)

data CloudFront = CloudFront
    { _baseUrl   :: URI
    , _keyPairId :: KeyPairId
    , _ttl       :: Word
    , _clock     :: IO POSIXTime
    , _func      :: ByteString -> IO (Either Error ByteString)
    }

initCloudFront :: MonadIO m => FilePath -> KeyPairId -> Word -> Domain -> m CloudFront
initCloudFront kfp kid ttl (Domain dom) = liftIO $
    CloudFront baseUrl kid ttl <$> mkPOSIXClock <*> sha1Rsa kfp
  where
    baseUrl = URI
        { uriScheme = Scheme "https"
        , uriAuthority = Just (Authority Nothing (Host (encodeUtf8 dom)) Nothing)
        , uriPath = "/"
        , uriQuery = Query []
        , uriFragment = Nothing
        }

signedURL :: (MonadIO m, ToByteString p) => CloudFront -> p -> m URI
signedURL (CloudFront base kid ttl clock sign) path = liftIO $ do
    time <- (+ ttl) . round <$> clock
    s    <- sign $ toStrict (toLazyByteString (policy url time))
    case s of
        Left e    -> throwM $ AWS.SigningError e
        Right sig -> return $! url
            { uriQuery = Query
                [ ("Expires", toByteString' time)
                , ("Signature", b64 sig)
                , ("Key-Pair-Id", toByteString' kid)
                ]
            }
  where
    url = base { uriPath = "/" <> toByteString' path }

    -- | Canned policy
    policy r t = "{\"Statement\":[{\"Resource\":\"" <> serializeURIRef r <> "\",\
                 \\"Condition\":{\
                    \\"DateLessThan\":{\
                        \\"AWS:EpochTime\":" <> wordDec t <>
                    "}}}]}"

    -- | AWS-specific URL-safe Base64 encoding
    b64 = C8.map f . B64.encode
      where
        f '+' = '-'
        f '=' = '_'
        f '/' = '~'
        f c   = c

sha1Rsa :: FilePath -> IO (ByteString -> IO (Either Error ByteString))
sha1Rsa fp = do
    kbs <- readKeyFile fp
    let key = case kbs of
                []               -> error $ "no keys found in " ++ show fp
                (PrivKeyRSA k:[]) -> k
                _                -> error $ "Not one RSA key found in " ++ show fp
    return (RSA.signSafer (Just SHA1) key)

mkPOSIXClock :: IO (IO POSIXTime)
mkPOSIXClock =
    mkAutoUpdate defaultUpdateSettings {
        updateAction = getPOSIXTime
    }
