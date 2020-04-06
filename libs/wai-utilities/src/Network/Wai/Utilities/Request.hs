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

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wno-orphans #-}
-- for "instance HasRequest Request" :(

module Network.Wai.Utilities.Request where

import Control.Error
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Lazy as Text
import Imports
import Network.HTTP.Types.Status (status400)
import Network.Wai
import Network.Wai.Predicate
import Network.Wai.Predicate.Request
import qualified Network.Wai.Utilities.Error as Wai
import Network.Wai.Utilities.ZAuth ((.&>))
import Pipes
import qualified Pipes.Prelude as P

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
    thrw msg = throwM $ Wai.Error status400 "bad-request" msg

parseOptionalBody ::
  (MonadIO m, FromJSON a) =>
  OptionalJsonRequest a ->
  ExceptT LText m (Maybe a)
parseOptionalBody r =
  hoistEither . fmapL Text.pack . traverse eitherDecode' . nonEmptyBody =<< readBody r
  where
    nonEmptyBody "" = Nothing
    nonEmptyBody ne = Just ne

lookupRequestId :: HasRequest r => r -> Maybe ByteString
lookupRequestId = lookup "Request-Id" . requestHeaders . getRequest

----------------------------------------------------------------------------
-- Typed JSON 'Request'

newtype JsonRequest body = JsonRequest {fromJsonRequest :: Request}

jsonRequest ::
  forall body r.
  (HasRequest r, HasHeaders r) =>
  Predicate r Error (JsonRequest body)
jsonRequest =
  contentType "application" "json"
    .&> (return . JsonRequest . getRequest)

newtype OptionalJsonRequest body = OptionalJsonRequest {fromOptionalJsonRequest :: Request}

optionalJsonRequest ::
  forall body r.
  (HasRequest r, HasHeaders r) =>
  Predicate r Error (OptionalJsonRequest body)
optionalJsonRequest =
  opt (contentType "application" "json")
    .&> (return . OptionalJsonRequest . getRequest)

----------------------------------------------------------------------------
-- Instances

instance HasRequest (JsonRequest a) where
  getRequest = fromJsonRequest

instance HasRequest (OptionalJsonRequest a) where
  getRequest = fromOptionalJsonRequest

instance HasRequest Request where
  getRequest = id
