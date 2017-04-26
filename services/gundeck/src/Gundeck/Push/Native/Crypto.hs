{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Push.Native.Crypto (CipherData (..), encrypt) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Gundeck.Types
import OpenSSL.EVP.Digest
import OpenSSL.EVP.Cipher
import OpenSSL.Random

data CipherData = CipherData
    { cipherMac  :: !ByteString
    , cipherData :: !ByteString
    } deriving (Eq, Show)

encrypt :: MonadIO m => ByteString -> SignalingKeys -> Cipher -> Digest -> m CipherData
encrypt plain (SignalingKeys (EncKey encKey) (MacKey macKey)) ciph dgst = liftIO $ do
    iv  <- randBytes 16
    enc <- (iv <>) <$> cipherBS ciph encKey iv Encrypt plain
    let mac = hmacBS dgst macKey enc
    return CipherData { cipherMac = mac, cipherData = enc }
