{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.Jwk where

import Control.Exception
import Crypto.JOSE.JWK
import Data.Aeson
import qualified Data.ByteString as BS
import Data.String.Conversions (cs)
import Debug.Trace (traceM)
import Imports
import Polysemy

data Jwk m a where
  Get :: FilePath -> Jwk m (Maybe JWK)

makeSem ''Jwk

interpretJwk :: Members '[Embed IO] r => Sem (Jwk ': r) a -> Sem r a
interpretJwk = interpret $ \(Get fp) -> liftIO $ readJwk fp

readJwk :: FilePath -> IO (Maybe JWK)
readJwk fp = do
  -- try @IOException (BS.readFile fp) <&> either (const Nothing) (decode . cs)
  bsOrError <- try @IOException $ BS.readFile fp
  case bsOrError of
    Left err -> traceM ("Failed to read file because: " <> show err) $> Nothing
    Right bs -> do
      traceM $ "File contents:\n" <> show bs
      case eitherDecode (cs bs) of
        Left err -> traceM ("Failed to decode file because: " <> show err) $> Nothing
        Right jwk -> pure $ Just jwk
