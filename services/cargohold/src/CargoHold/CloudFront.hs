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
    , signedUrl
    ) where

import Control.AutoUpdate
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Conversion
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX
import Data.Yaml (FromJSON)
import GHC.Generics
import OpenSSL.EVP.Digest (getDigestByName)
import OpenSSL.PEM (readPrivateKey, PemPasswordSupply (PwNone))
import URI.ByteString

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as C8
import qualified OpenSSL.EVP.Sign       as SSL

newtype KeyPairId = KeyPairId Text
    deriving (Eq, Show, ToByteString, Generic, FromJSON)

newtype Domain = Domain Text
    deriving (Eq, Show, ToByteString, Generic, FromJSON)

data CloudFront = CloudFront
    { _baseUrl   :: URI
    , _keyPairId :: KeyPairId
    , _clock     :: IO POSIXTime
    , _func      :: ByteString -> IO ByteString
    }

initCloudFront :: MonadIO m => FilePath -> KeyPairId -> Domain -> m CloudFront
initCloudFront kfp kid (Domain dom) = liftIO $
    CloudFront baseUrl kid <$> mkPOSIXClock <*> sha1Rsa kfp
  where
    baseUrl = URI
        { uriScheme = Scheme "https"
        , uriAuthority = Just (Authority Nothing (Host (encodeUtf8 dom)) Nothing)
        , uriPath = "/"
        , uriQuery = Query []
        , uriFragment = Nothing
        }

signedUrl :: (MonadIO m, ToByteString p) => CloudFront -> p -> m URI
signedUrl (CloudFront base kid clock sign) path = liftIO $ do
    time <- (+ 300) . round <$> clock
    sig  <- sign (toStrict (toLazyByteString (policy url time)))
    return $! url
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

sha1Rsa :: FilePath -> IO (ByteString -> IO ByteString)
sha1Rsa fp = do
    sha1 <- liftIO $ getDigestByName "SHA1" >>=
                maybe (error "OpenSSL: SHA1 not found") return
    kbs  <- readFile fp
    key  <- readPrivateKey kbs PwNone
    return (SSL.signBS sha1 key)

mkPOSIXClock :: IO (IO POSIXTime)
mkPOSIXClock =
    mkAutoUpdate defaultUpdateSettings {
        updateAction = getPOSIXTime
    }

