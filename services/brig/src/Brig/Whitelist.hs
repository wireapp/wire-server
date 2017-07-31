{-# LANGUAGE OverloadedStrings #-}

module Brig.Whitelist (Whitelist (..), verify) where

import Bilge.IO
import Bilge.Request
import Bilge.Response
import Bilge.Retry
import Brig.Types
import Control.Monad.Catch (MonadMask, throwM)
import Control.Monad.IO.Class
import Control.Retry
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))

data Whitelist = Whitelist
    { whitelistReq  :: !Request
    , whitelistUser :: !ByteString
    , whitelistPass :: !ByteString
    } deriving Show

verify :: (MonadIO m, MonadMask m, MonadHttp m) => Whitelist -> Either Email Phone -> m Bool
verify (Whitelist rq user pass) key =
    recovering x3 httpHandlers . const $ do
        rsp <- get' rq (req user pass)
        case statusCode rsp of
            200 -> return True
            404 -> return False
            _   -> throwM $
                HttpExceptionRequest rq (StatusCodeException (rsp { responseBody = () }) mempty)
  where
    urlEmail = queryItem "email" . encodeUtf8 . fromEmail
    urlPhone = queryItem "mobile" . encodeUtf8 . fromPhone

    req u p = port 443 . secure
            . either urlEmail urlPhone key
            . applyBasicAuth u p

    x3 = limitRetries 3 <> exponentialBackoff 100000
