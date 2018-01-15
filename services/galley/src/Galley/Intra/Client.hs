{-# LANGUAGE OverloadedStrings #-}

module Galley.Intra.Client
    ( lookupClients
    ) where

import Bilge hiding (options, getHeader, statusCode)
import Bilge.RPC
import Galley.App
import Galley.Intra.Util
import Galley.Types (UserClients)
import Data.Id
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

lookupClients :: [UserId] -> Galley UserClients
lookupClients uids = do
    (h, p) <- brigReq
    r <- call "brig"
        $ method POST . host h . port p
        . path "/i/clients"
        . json uids
        . expect2xx
    parseResponse (Error status502 "server-error") r
