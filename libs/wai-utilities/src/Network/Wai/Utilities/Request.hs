{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Network.Wai.Utilities.Request where

import Control.Applicative
import Control.Error
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)
import Network.Wai
import Pipes
import Prelude

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Lazy       as Text
import qualified Pipes.Prelude        as P

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
          -> ExceptT Text m a
#else
          -> EitherT Text m a
#endif
parseBody r = readBody r >>= hoistEither . fmapL Text.pack . eitherDecode'

lookupRequestId :: Request -> Maybe ByteString
lookupRequestId = lookup "Request-Id" . requestHeaders
