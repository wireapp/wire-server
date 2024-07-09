{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

-- orphan instance for "instance HasRequest Request" :(

module Network.Wai.Utilities.Request where

import Control.Error
import Data.Aeson (FromJSON, eitherDecode')
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as Lazy
import Data.Id (RequestId (RequestId))
import Data.Text.Lazy qualified as Text
import Imports
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request (requestHeaders), getRequestBodyChunk)
import Network.Wai.Predicate.Request (HasRequest (..))
import Pipes (yield)
import Pipes.Prelude qualified as P

readBody :: (MonadIO m, HasRequest r) => r -> m LByteString
readBody r = liftIO $ Lazy.fromChunks <$> P.toListM chunks
  where
    chunks = do
      b <- lift $ getRequestBodyChunk (getRequest r)
      unless (B.null b) $ do
        yield b
        chunks

parseBody ::
  (MonadIO m, FromJSON a) =>
  JsonRequest a ->
  ExceptT LText m a
parseBody r = readBody r >>= hoistEither . fmapL Text.pack . eitherDecode'

lookupRequestId :: HeaderName -> Request -> Maybe ByteString
lookupRequestId reqIdHeaderName =
  lookup reqIdHeaderName . requestHeaders

getRequestId :: HeaderName -> Request -> RequestId
getRequestId reqIdHeaderName req =
  RequestId $ fromMaybe "N/A" $ lookupRequestId reqIdHeaderName req

----------------------------------------------------------------------------
-- Typed JSON 'Request'

newtype JsonRequest body = JsonRequest {fromJsonRequest :: Request}

newtype OptionalJsonRequest body = OptionalJsonRequest {fromOptionalJsonRequest :: Request}

----------------------------------------------------------------------------
-- Instances

instance HasRequest (JsonRequest a) where
  getRequest = fromJsonRequest

instance HasRequest (OptionalJsonRequest a) where
  getRequest = fromOptionalJsonRequest

instance HasRequest Request where
  getRequest = id
