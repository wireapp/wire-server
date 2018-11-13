{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Brig.Whitelist (Whitelist (..), verify) where

import Imports
import Data.Aeson
import Bilge.IO
import Bilge.Request
import Bilge.Response
import Bilge.Retry
import Brig.Types
import Control.Monad.Catch (MonadMask, throwM)
import Control.Retry
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..), parseRequest)

data Whitelist = Whitelist
    { whitelistUrl  :: !Text
    , whitelistUser :: !Text
    , whitelistPass :: !Text
    } deriving (Show, Generic)

instance FromJSON Whitelist

verify :: (MonadIO m, MonadMask m, MonadHttp m) => Whitelist -> Either Email Phone -> m Bool
verify (Whitelist url user pass) key =
    recovering x3 httpHandlers . const $ do
        rq <- parseRequest $ unpack url
        rsp <- get' rq $ req (encodeUtf8 user) (encodeUtf8 pass)
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
