{-# LANGUAGE OverloadedStrings #-}

module Galley.Intra.Util
    ( brigReq
    , call
    , x1
    ) where

import Bilge hiding (options, getHeader, statusCode)
import Bilge.RPC
import Bilge.Retry
import Galley.App
import Galley.Options
import Control.Lens (view)
import Control.Retry
import Data.ByteString (ByteString)
import Data.Misc (portNumber)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16)
import Util.Options

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT


brigReq :: Galley (ByteString, Word16)
brigReq = do
    h <- encodeUtf8 <$> view (options.optBrig.epHost)
    p <- portNumber . fromIntegral <$> view (options.optBrig.epPort)
    return (h, p)

call :: LT.Text -> (Request -> Request) -> Galley (Response (Maybe LB.ByteString))
call n r = recovering x1 rpcHandlers (const (rpc n r))

x1 :: RetryPolicy
x1 = limitRetries 1
