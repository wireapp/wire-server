{-# LANGUAGE DeriveDataTypeable #-}

module Bonanza.Streaming.Binary
  ( ParseError (..),
    decode,
  )
where

import Control.Monad.Catch
import Data.Binary.Get
import qualified Data.ByteString as BS
import Data.Conduit
import Imports

data ParseError = ParseError ByteString ByteOffset String
  deriving (Show, Typeable)

instance Exception ParseError

decode :: MonadThrow m => Get o -> ConduitT ByteString o m ()
decode g = start
  where
    start = do
      mx <- await
      case mx of
        Nothing -> return ()
        Just x -> go (runGetIncremental g `pushChunk` x)
    go (Fail u o e) = throwM $ ParseError u o e
    go (Partial n) = await >>= go . n
    go (Done bs _ v)
      | BS.null bs = yield v *> start
      | otherwise = yield v *> go (runGetIncremental g `pushChunk` bs)
