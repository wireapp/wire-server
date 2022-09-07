{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.PublicKeyBundle where

import Control.Exception
import Data.ByteString.Conversion
import Data.PEMKeys
import Data.String.Conversions (cs)
import Imports
import Polysemy

data PublicKeyBundle m a where
  Get :: FilePath -> PublicKeyBundle m (Maybe PEMKeys)

makeSem ''PublicKeyBundle

interpretPublicKeyBundle :: Members '[Embed IO] r => Sem (PublicKeyBundle ': r) a -> Sem r a
interpretPublicKeyBundle = interpret $ \(Get fp) -> do
  contents <- liftIO $ try $ readFile fp
  pure $ case contents of
    Left (_ :: IOException) -> Nothing
    Right pem -> fromByteString $ cs pem
