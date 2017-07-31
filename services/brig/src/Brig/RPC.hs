{-# LANGUAGE OverloadedStrings #-}

-- | General RPC utilities.
module Brig.RPC where

import Bilge
import Bilge.Retry
import Bilge.RPC
import Brig.App
import Control.Exception
import Control.Lens (view)
import Control.Monad (when)
import Control.Monad.Catch
import Control.Retry
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Conversion
import Data.Id
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), checkResponse)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import System.Logger.Class hiding ((.=), name)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as Text

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

remote :: ByteString -> Msg -> Msg
remote = field "remote"

decodeBody :: (FromJSON a, MonadThrow m) => Text -> Response (Maybe BL.ByteString) -> m a
decodeBody rm rs = case responseBody rs of
    Nothing -> throwM $ ParseException rm "Missing response body."
    Just bs -> decodeBytes rm bs

decodeBytes :: (FromJSON a, MonadThrow m) => Text -> BL.ByteString -> m a
decodeBytes ctx bs = case eitherDecode' bs of
    Left  e -> throwM $ ParseException ctx e
    Right a -> return a

expect :: [Status] -> Request -> Request
expect ss rq = rq { checkResponse = check }
  where
    check rq' rs = do
        let s   = responseStatus rs
            rs' = rs { responseBody = () }
        when (statusIsServerError s || s `notElem` ss) $
            throwM $ HttpExceptionRequest rq' (StatusCodeException rs' mempty)

galleyRequest :: StdMethod
              -> (Request -> Request)
              -> AppIO (Response (Maybe BL.ByteString))
galleyRequest m r = do
    g <- view galley
    recovering x3 rpcHandlers $ const $
        rpc' "galley" g (method m . r)

gundeckRequest :: StdMethod
               -> (Request -> Request)
               -> AppIO (Response (Maybe BL.ByteString))
gundeckRequest m r = do
    g <- view gundeck
    recovering x3 rpcHandlers $ const $
        rpc' "gundeck" g (method m . r)

-- | Failed to parse a response from another service.
data ParseException = ParseException
    { _parseExceptionRemote :: !Text
    , _parseExceptionMsg    :: String
    }

instance Show ParseException where
    show (ParseException r m) =
        "Failed to parse response from remote "
      ++ Text.unpack r ++ " with message: " ++ m

instance Exception ParseException

