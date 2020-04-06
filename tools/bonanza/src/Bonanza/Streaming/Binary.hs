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
