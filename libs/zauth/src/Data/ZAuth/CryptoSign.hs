{-# LANGUAGE TemplateHaskell #-}

module Data.ZAuth.CryptoSign where

import Imports
import Polysemy
import Sodium.Crypto.Sign as Crypto

data CryptoSign m a where
  Sign :: SecretKey -> ByteString -> CryptoSign m Signature
  VerifyWith :: PublicKey -> Signature -> ByteString -> CryptoSign m Bool

makeSem ''CryptoSign

runCryptoSign :: (Member (Embed IO) r) => InterpreterFor CryptoSign r
runCryptoSign = interpret $ \case
  Sign key bs -> embed $ signature key bs
  VerifyWith key sig bs -> embed $ Crypto.verifyWith key sig bs
