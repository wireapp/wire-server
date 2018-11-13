{-# LANGUAGE OverloadedStrings #-}

module Brig.Password
    ( Password
    , genPassword
    , mkSafePassword
    , verifyPassword
    ) where

import Imports
import Cassandra
import Crypto.Scrypt
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Misc (PlainTextPassword (..))
import OpenSSL.Random (randBytes)

import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding     as Text

-- | A derived, stretched password that can be safely stored.
newtype Password = Password
    { fromPassword :: EncryptedPass }

instance Show Password where
    show _ = "<Password>"

instance Cql Password where
    ctype = Tagged BlobColumn

    fromCql (CqlBlob lbs) = return . Password . EncryptedPass $ toStrict lbs
    fromCql _             = fail "password: expected blob"

    toCql = CqlBlob . fromStrict . getEncryptedPass . fromPassword

-- | Generate a strong, random plaintext password of length 16
-- containing only alphanumeric characters, '+' and '/'.
genPassword :: MonadIO m => m PlainTextPassword
genPassword = liftIO . fmap (PlainTextPassword . Text.decodeUtf8 . B64.encode)
            $ randBytes 12

-- | Stretch a plaintext password so that it can be safely stored.
mkSafePassword :: MonadIO m => PlainTextPassword -> m Password
mkSafePassword = liftIO . fmap Password . encryptPassIO' . pass
  where
    pass = Pass . Text.encodeUtf8 . fromPlainTextPassword

-- | Verify a plaintext password from user input against a stretched
-- password from persistent storage.
verifyPassword :: PlainTextPassword -> Password -> Bool
verifyPassword plain opaque =
    let actual   = Pass . Text.encodeUtf8 $ fromPlainTextPassword plain
        expected = fromPassword opaque
    in verifyPass' actual expected
