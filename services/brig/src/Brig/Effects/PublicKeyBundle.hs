{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.PublicKeyBundle where

import Data.ByteString.Conversion
import Data.PEMKeys
import Imports
import Polysemy

data PublicKeyBundle m a where
  Get :: FilePath -> PublicKeyBundle m (Maybe PEMKeys)

makeSem ''PublicKeyBundle

interpretPublicKeyBundleStub :: Members '[Embed IO] r => Sem (PublicKeyBundle ': r) a -> Sem r a
interpretPublicKeyBundleStub = interpret $ \(Get _) -> pure $ fromByteString pem
  where
    pem :: ByteString
    pem =
      ""
        <> "-----BEGIN PRIVATE KEY-----"
        <> "MC4CAQAwBQYDK2VwBCIEIFANnxZLNE4p+GDzWzR3wm/v8x/0bxZYkCyke1aTRucX"
        <> "-----END PRIVATE KEY-----"
        <> "-----BEGIN PUBLIC KEY-----"
        <> "MCowBQYDK2VwAyEACPvhIdimF20tOPjbb+fXJrwS2RKDp7686T90AZ0+Th8="
        <> "-----END PUBLIC KEY-----"
