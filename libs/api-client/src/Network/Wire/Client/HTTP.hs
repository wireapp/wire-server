{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Network.Wire.Client.HTTP
  ( clientRequest,
    readBody,
    unexpected,
    mkErrorResponse,
  )
where

import Bilge
import Control.Error
import Control.Monad.Catch
import Control.Retry
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Lazy as Lazy
import Data.List.NonEmpty (NonEmpty)
import Data.Text (pack)
import qualified Data.Text.Encoding as T
import Imports
import qualified Network.HTTP.Client as Rq
import Network.HTTP.Types.Header (hUserAgent)
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.Monad
import qualified System.Logger.Class as Log
import UnliftIO.Exception (throwIO)

data Error
  = Error
      { code :: Int,
        label :: Text,
        message :: Text
      }
  deriving (Show)

instance FromJSON Error where
  parseJSON = withObject "error" $ \o ->
    Error <$> o .: "code"
      <*> o .: "label"
      <*> o .: "message"

-------------------------------------------------------------------------------
-- Performing Requests

clientRequest ::
  forall m a.
  (Log.MonadLogger m, MonadClient m, MonadUnliftIO m, MonadMask m) =>
  -- | The request to send.
  Request ->
  -- | Expected response codes.
  NonEmpty Status ->
  -- | Handler function.
  (Response BodyReader -> IO a) ->
  m a
clientRequest rq expected f = do
  recovering retry3x handlers (const exec)
  where
    idempotent = Rq.method rq `elem` ["GET", "PUT", "DELETE", "HEAD", "OPTIONS"]
    canRetry c = idempotent && c `elem` [408, 420, 500, 502, 503, 504]
    retry3x = limitRetries 3 <> exponentialBackoff 1000000
    handlers :: [RetryStatus -> Handler m Bool]
    handlers =
      [ const $
          Handler
            ( \(e :: ClientException) -> case e of
                ErrorResponse c _ _ -> return (canRetry c)
                x -> throwIO x
            ),
        const $ Handler (\(e :: SomeException) -> throwIO e)
      ]
    exec :: m a
    exec = do
      s <- getServer
      let rq' =
            rq & setServer s
              & header hUserAgent "api-client"
      Log.debug $ Log.msg (show rq')
      runInIO <- askRunInIO
      handleRequestWithCont rq' $ \rs -> do
        runInIO $ Log.debug $ Log.msg $ show (rs {responseBody = "" :: String})
        if responseStatus rs `elem` toList expected
          then f rs
          else
            if (statusCode rs `div` 100) `elem` [4, 5]
              then mkErrorResponse rs >>= throwIO
              else unexpected rs ""

-------------------------------------------------------------------------------
-- Utilities

readBody :: (Typeable a, FromJSON a) => Response BodyReader -> IO a
readBody = consumeBody >=> responseJsonThrow (ParseError . pack)

unexpected :: MonadIO m => Response a -> Text -> m b
unexpected r = liftIO . throwIO . UnexpectedResponse (responseStatus r) (responseHeaders r)

mkErrorResponse :: Response BodyReader -> IO ClientException
mkErrorResponse rs = do
  r <- consumeBody rs
  let re =
        maybe
          (Left "N/A")
          ( \bdy ->
              fmapL
                (const . T.decodeLatin1 $ Lazy.toStrict bdy)
                (eitherDecode bdy)
          )
          (responseBody r)
  return $ case re of
    Left m -> ErrorResponse (statusCode rs) "N/A" m
    Right e -> ErrorResponse (code e) (label e) (message e)
