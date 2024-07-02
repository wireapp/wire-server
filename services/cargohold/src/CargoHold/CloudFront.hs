{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module CargoHold.CloudFront
  ( CloudFront,
    Domain (..),
    KeyPairId (..),
    initCloudFront,
    signedURL,
  )
where

import Control.AutoUpdate
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX
import Data.Yaml (FromJSON)
import Imports
import OpenSSL.EVP.Digest (getDigestByName)
import qualified OpenSSL.EVP.Sign as SSL
import OpenSSL.PEM (PemPasswordSupply (PwNone), readPrivateKey)
import URI.ByteString

newtype KeyPairId = KeyPairId Text
  deriving (Eq, Show, ToByteString, Generic, FromJSON)

newtype Domain = Domain Text
  deriving (Eq, Show, ToByteString, Generic, FromJSON)

data CloudFront = CloudFront
  { _baseUrl :: URI,
    _keyPairId :: KeyPairId,
    _ttl :: Word,
    _clock :: IO POSIXTime,
    _func :: ByteString -> IO ByteString
  }

initCloudFront :: (MonadIO m) => FilePath -> KeyPairId -> Word -> Domain -> m CloudFront
initCloudFront kfp kid ttl (Domain dom) =
  liftIO $
    CloudFront baseUrl kid ttl <$> mkPOSIXClock <*> sha1Rsa kfp
  where
    baseUrl =
      URI
        { uriScheme = Scheme "https",
          uriAuthority = Just (Authority Nothing (Host (encodeUtf8 dom)) Nothing),
          uriPath = "/",
          uriQuery = Query [],
          uriFragment = Nothing
        }

signedURL :: (MonadIO m, ToByteString p) => CloudFront -> p -> m URI
signedURL (CloudFront base kid ttl clock sign) path = liftIO $ do
  time <- (+ ttl) . round <$> clock
  sig <- sign (toStrict (toLazyByteString (policy url time)))
  pure $!
    url
      { uriQuery =
          Query
            [ ("Expires", toByteString' time),
              ("Signature", b64 sig),
              ("Key-Pair-Id", toByteString' kid)
            ]
      }
  where
    url = base {uriPath = "/" <> toByteString' path}
    policy r t =
      "{\"Statement\":[{\"Resource\":\""
        <> serializeURIRef r
        <> "\",\
           \\"Condition\":{\
           \\"DateLessThan\":{\
           \\"AWS:EpochTime\":"
        <> wordDec t
        <> "}}}]}"
    b64 = C8.map f . B64.encode
      where
        f '+' = '-'
        f '=' = '_'
        f '/' = '~'
        f c = c

sha1Rsa :: FilePath -> IO (ByteString -> IO ByteString)
sha1Rsa fp = do
  sha1 <-
    liftIO $
      getDigestByName "SHA1"
        >>= maybe (error "OpenSSL: SHA1 not found") pure
  kbs <- readFile fp
  key <- readPrivateKey kbs PwNone
  pure (SSL.signBS sha1 key)

mkPOSIXClock :: IO (IO POSIXTime)
mkPOSIXClock =
  mkAutoUpdate
    defaultUpdateSettings
      { updateAction = getPOSIXTime
      }
