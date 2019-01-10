{-# LANGUAGE OverloadedStrings #-}

module Galley.Intra.Util
    ( brigReq
    , sparReq
    , call
    , x1
    ) where

import Imports
import Bilge hiding (options, getHeader, statusCode)
import Bilge.RPC
import Bilge.Retry
import Galley.App
import Galley.Options
import Control.Lens (view)
import Control.Retry
import Data.Misc (portNumber)
import Data.Text.Encoding (encodeUtf8)
import Util.Options

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT


brigReq :: Galley (ByteString, Word16)
brigReq = do
    h <- encodeUtf8 <$> view (options.optBrig.epHost)
    p <- portNumber . fromIntegral <$> view (options.optBrig.epPort)
    return (h, p)

sparReq :: Galley (ByteString, Word16)
sparReq = do
    h <- encodeUtf8 <$> view (options.optSpar.epHost)
    p <- portNumber . fromIntegral <$> view (options.optSpar.epPort)
    return (h, p)

-- gundeckReq lives in Galley.Intra.Push

call :: LT.Text -> (Request -> Request) -> Galley (Response (Maybe LB.ByteString))
call n r = recovering x1 rpcHandlers (const (rpc n r))

x1 :: RetryPolicy
x1 = limitRetries 1
