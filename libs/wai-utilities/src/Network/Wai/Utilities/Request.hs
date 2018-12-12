{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Network.Wai.Utilities.Request where

import Imports
import Control.Error
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson
import Network.HTTP.Types.Status (status400)
import Network.Wai
import Pipes

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Lazy       as Text
import qualified Pipes.Prelude        as P
import qualified Network.Wai.Utilities.Error as Wai

readBody :: MonadIO m => Request -> m Lazy.ByteString
readBody r = liftIO $ Lazy.fromChunks <$> P.toListM chunks
  where
    chunks = do
        b <- lift $ requestBody r
        unless (B.null b) $ do
            yield b
            chunks

parseBody :: (MonadIO m, FromJSON a)
          => Request
#if MIN_VERSION_errors(2,0,0)
          -> ExceptT LText m a
#else
          -> EitherT LText m a
#endif
parseBody r = readBody r >>= hoistEither . fmapL Text.pack . eitherDecode'

parseBody' :: (FromJSON a, MonadIO m, MonadThrow m) => Request -> m a
parseBody' r = either thrw pure =<< runExceptT (parseBody r)
  where
    thrw msg = throwM $ Wai.Error status400 "bad-request" msg

lookupRequestId :: Request -> Maybe ByteString
lookupRequestId = lookup "Request-Id" . requestHeaders
