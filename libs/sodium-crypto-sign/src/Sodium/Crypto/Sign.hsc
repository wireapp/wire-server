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

{-# LANGUAGE CPP #-}

module Sodium.Crypto.Sign
    ( PublicKey (..)
    , SecretKey (..)
    , Signature (..)
    , newKeyPair
    , sign
    , signature
    , signatureLength
    , verify
    , verifyWith
    ) where

import Imports
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base64.URL
import Foreign hiding (void)
import Foreign.C

import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe   as U

newtype PublicKey = PublicKey { pubBytes :: ByteString } deriving (Eq, Ord)
newtype SecretKey = SecretKey { secBytes :: ByteString } deriving (Eq, Ord)
newtype Signature = Signature { sigBytes :: ByteString } deriving (Eq, Ord)

instance Read PublicKey where
    readsPrec _ = either error (\k -> [(PublicKey k, "")]) . decode . pack

instance Read SecretKey where
    readsPrec _ = either error (\k -> [(SecretKey k, "")]) . decode . pack

instance Read Signature where
    readsPrec _ = either error (\k -> [(Signature k, "")]) . decode . pack

instance Show PublicKey where
    show = unpack . encode . pubBytes

instance Show SecretKey where
    show = unpack . encode . secBytes

instance Show Signature where
    show = unpack . encode . sigBytes

-- | Please note that this function is not thread-safe.
newKeyPair :: IO (PublicKey, SecretKey)
newKeyPair = do
    pl <- fromIntegral <$> publicKeyLength
    sl <- fromIntegral <$> secretKeyLength
    pk <- I.mallocByteString pl
    sk <- I.mallocByteString sl

    withForeignPtr pk $ \ppk ->
        withForeignPtr sk $ \psk ->
            void $ c_crypto_sign_keypair (castPtr ppk) (castPtr psk)

    return ( PublicKey (I.fromForeignPtr pk 0 pl)
           , SecretKey (I.fromForeignPtr sk 0 sl) )

sign :: SecretKey -> ByteString -> IO ByteString
sign k b = do
    siglen <- fromIntegral <$> signatureLength
    U.unsafeUseAsCStringLen b $ \(m, mlen) ->
        U.unsafeUseAsCString (secBytes k) $ \sk ->
            I.createAndTrim (mlen + siglen) $ \sm ->
                alloca $ \smlen -> do
                    void $ c_crypto_sign (castPtr sm) smlen (castPtr m) (fromIntegral mlen) (castPtr sk)
                    fromIntegral <$> peek smlen

signature :: SecretKey -> ByteString -> IO Signature
signature k m = do
    sm <- sign k m
    return $ Signature (B.take (B.length sm - B.length m) sm)

verify :: PublicKey -> ByteString -> IO Bool
verify k m =
    U.unsafeUseAsCStringLen m $ \(ms, mslen) ->
        U.unsafeUseAsCString (pubBytes k) $ \pk ->
            alloca $ \pmlen -> do
                out <- I.mallocByteString mslen
                res <- withForeignPtr out $ \pout ->
                    c_crypto_sign_open (castPtr pout) pmlen (castPtr ms) (fromIntegral mslen) (castPtr pk)
                return (res == 0)

verifyWith :: PublicKey -> Signature -> ByteString -> IO Bool
verifyWith k s m = verify k (sigBytes s <> m)

secretKeyLength :: IO Word
secretKeyLength = fromIntegral <$> c_crypto_sign_secretkeybytes
{-# INLINE secretKeyLength #-}

publicKeyLength :: IO Word
publicKeyLength = fromIntegral <$> c_crypto_sign_publickeybytes
{-# INLINE publicKeyLength #-}

signatureLength :: IO Word
signatureLength = fromIntegral <$> c_crypto_sign_bytes
{-# INLINE signatureLength #-}

-----------------------------------------------------------------------------
-- FFI

#include <sodium/crypto_sign.h>

foreign import ccall unsafe "crypto_sign.h crypto_sign_bytes"
    c_crypto_sign_bytes :: IO CSize

foreign import ccall unsafe "crypto_sign.h crypto_sign_publickeybytes"
    c_crypto_sign_publickeybytes :: IO CSize

foreign import ccall unsafe "crypto_sign.h crypto_sign_secretkeybytes"
    c_crypto_sign_secretkeybytes :: IO CSize

foreign import ccall unsafe "crypto_sign.h crypto_sign_keypair"
    c_crypto_sign_keypair :: Ptr CUChar -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "crypto_sign.h crypto_sign"
    c_crypto_sign :: Ptr CUChar  -- signed message
                  -> Ptr CULLong -- signed message length
                  -> Ptr CUChar  -- plain text message
                  -> CULLong     -- plain text length
                  -> Ptr CUChar  -- secret key
                  -> IO CInt

foreign import ccall unsafe "crypto_sign.h crypto_sign_open"
    c_crypto_sign_open :: Ptr CUChar  -- plain text message
                       -> Ptr CULLong -- plain text message length
                       -> Ptr CUChar  -- signed message
                       -> CULLong     -- signed message length
                       -> Ptr CUChar  -- public key
                       -> IO CInt
