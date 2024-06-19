{-# LANGUAGE TemplateHaskell #-}

module Wire.Sem.Jwk where

import Control.Exception
import Crypto.JOSE.JWK
import Data.Aeson
import Data.ByteString (fromStrict)
import qualified Data.ByteString as BS
import Imports
import Polysemy

data Jwk m a where
  Get :: FilePath -> Jwk m (Maybe JWK)

makeSem ''Jwk

interpretJwk :: (Members '[Embed IO] r) => Sem (Jwk ': r) a -> Sem r a
interpretJwk = interpret $ \(Get fp) -> liftIO $ readJwk fp

readJwk :: FilePath -> IO (Maybe JWK)
readJwk fp =
  try @IOException (BS.readFile fp)
    <&> either
      (const Nothing)
      (decode . fromStrict)
