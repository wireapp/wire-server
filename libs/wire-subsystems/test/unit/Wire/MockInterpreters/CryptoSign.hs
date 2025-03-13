{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Wire.MockInterpreters.CryptoSign where

import Data.ZAuth.CryptoSign (CryptoSign)
import Data.ZAuth.CryptoSign qualified as CryptoSign
import Imports
import Polysemy
import Sodium.Crypto.Sign qualified as Sodium
import System.IO.Unsafe (unsafePerformIO)

-- Perhaps it is also possible to implment this with crypton, but EdDSA with
-- Curve_Edwards25519 uses secret key size of 32, while the libsodium has secret
-- key size of 64.
runCryptoSignUnsafe :: InterpreterFor CryptoSign r
runCryptoSignUnsafe = interpret \case
  CryptoSign.Sign sec msg -> do
    pure $ unsafePerformIO $ Sodium.signature sec msg
  CryptoSign.VerifyWith pubKey sig msg -> do
    pure $ unsafePerformIO $ Sodium.verifyWith pubKey sig msg
