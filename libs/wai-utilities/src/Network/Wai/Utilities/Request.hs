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
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as Lazy
import Data.Id
import Data.Text.Lazy qualified as Text
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate
import Network.Wai.Predicate.Request
import Network.Wai.Utilities.Error qualified as Wai
import Network.Wai.Utilities.ZAuth ((.&>))
import Pipes
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

parseBody' :: (FromJSON a, MonadIO m, MonadThrow m) => JsonRequest a -> m a
parseBody' r = either thrw pure =<< runExceptT (parseBody r)
  where
    thrw msg = throwM $ Wai.mkError status400 "bad-request" msg

parseOptionalBody ::
  (MonadIO m, FromJSON a) =>
  OptionalJsonRequest a ->
  ExceptT LText m (Maybe a)
parseOptionalBody r =
  hoistEither . fmapL Text.pack . traverse eitherDecode' . nonEmptyBody =<< readBody r
  where
    nonEmptyBody "" = Nothing
    nonEmptyBody ne = Just ne

lookupRequestId :: HeaderName -> Request -> Maybe ByteString
lookupRequestId reqIdHeaderName =
  lookup reqIdHeaderName . requestHeaders

getRequestId :: HeaderName -> Request -> RequestId
getRequestId reqIdHeaderName req =
  RequestId $ fromMaybe "N/A" $ lookupRequestId reqIdHeaderName req

----------------------------------------------------------------------------
-- Typed JSON 'Request'

newtype JsonRequest body = JsonRequest {fromJsonRequest :: Request}

jsonRequest ::
  forall body r.
  (HasRequest r, HasHeaders r) =>
  Predicate r Error (JsonRequest body)
jsonRequest =
  contentType "application" "json"
    .&> (pure . JsonRequest . getRequest)

newtype OptionalJsonRequest body = OptionalJsonRequest {fromOptionalJsonRequest :: Request}

optionalJsonRequest ::
  forall body r.
  (HasRequest r, HasHeaders r) =>
  Predicate r Error (OptionalJsonRequest body)
optionalJsonRequest =
  opt (contentType "application" "json")
    .&> (pure . OptionalJsonRequest . getRequest)

----------------------------------------------------------------------------
-- Instances

instance HasRequest (JsonRequest a) where
  getRequest = fromJsonRequest

instance HasRequest (OptionalJsonRequest a) where
  getRequest = fromOptionalJsonRequest

instance HasRequest Request where
  getRequest = id
