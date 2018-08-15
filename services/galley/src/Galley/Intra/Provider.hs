{-# LANGUAGE OverloadedStrings #-}

module Galley.Intra.Provider
    ( queryServiceWhitelist
    ) where

import Bilge hiding (options, getHeader, statusCode)
import Bilge.RPC
import Galley.App
import Galley.Intra.Util
import Galley.Types.Bot
import Data.ByteString.Conversion
import Data.Id
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

queryServiceWhitelist :: TeamId -> [ServiceRef] -> Galley [Bool]
queryServiceWhitelist tid svcs = do
    (h, p) <- brigReq
    r <- call "brig"
        $ method POST . host h . port p
        . paths ["/i/teams", toByteString' tid, "services/whitelisted"]
        . json svcs
        . expect2xx
    parseResponse (Error status502 "server-error") r
