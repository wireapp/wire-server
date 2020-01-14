{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

{-# OPTIONS -Wno-orphans #-}  -- for "instance HasRequest Request" :(

module Network.Wai.Utilities.Request where

import Imports
import Control.Error
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson
import Network.HTTP.Types.Status (status400)
import Network.Wai
import Network.Wai.Predicate
import Network.Wai.Predicate.Request
import Network.Wai.Utilities.ZAuth ((.&>))
import Pipes

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Lazy       as Text
import qualified Pipes.Prelude        as P
import qualified Network.Wai.Utilities.Error as Wai

readBody :: (MonadIO m, HasRequest r) => r -> m LByteString
readBody r = liftIO $ Lazy.fromChunks <$> P.toListM chunks
  where
    chunks = do
        b <- lift $ getRequestBodyChunk (getRequest r)
        unless (B.null b) $ do
            yield b
            chunks

parseBody :: (MonadIO m, FromJSON a)
          => JsonRequest a
          -> ExceptT LText m a
parseBody r = readBody r >>= hoistEither . fmapL Text.pack . eitherDecode'

parseBody' :: (FromJSON a, MonadIO m, MonadThrow m) => JsonRequest a -> m a
parseBody' r = either thrw pure =<< runExceptT (parseBody r)
  where
    thrw msg = throwM $ Wai.Error status400 "bad-request" msg

lookupRequestId :: HasRequest r => r -> Maybe ByteString
lookupRequestId = lookup "Request-Id" . requestHeaders . getRequest

----------------------------------------------------------------------------
-- Typed JSON 'Request'

newtype JsonRequest body = JsonRequest { fromJsonRequest :: Request }

jsonRequest
    :: forall body r. (HasRequest r, HasHeaders r)
    => Predicate r Error (JsonRequest body)
jsonRequest =
    contentType "application" "json"
    .&> (return . JsonRequest . getRequest)

----------------------------------------------------------------------------
-- Instances

instance HasRequest (JsonRequest a) where
    getRequest = fromJsonRequest

instance HasRequest Request where
    getRequest = id
