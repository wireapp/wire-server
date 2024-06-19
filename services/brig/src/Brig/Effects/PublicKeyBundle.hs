{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.PublicKeyBundle where

import Control.Exception
import Data.ByteString qualified as BS
import Data.ByteString.Conversion
import Data.PEMKeys
import Imports
import Polysemy

data PublicKeyBundle m a where
  Get :: FilePath -> PublicKeyBundle m (Maybe PEMKeys)

makeSem ''PublicKeyBundle

interpretPublicKeyBundle :: (Member (Embed IO) r) => Sem (PublicKeyBundle ': r) a -> Sem r a
interpretPublicKeyBundle = interpret $ \(Get fp) -> do
  contents :: Either IOException ByteString <- liftIO $ try $ BS.readFile fp
  pure $ either (const Nothing) fromByteString contents
