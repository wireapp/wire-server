{-# LANGUAGE OverloadedStrings #-}

module Galley.Intra.Client
    ( lookupClients
    ) where

import Bilge hiding (options, getHeader, statusCode)
import Bilge.RPC
import Brig.Types.Intra
import Galley.App
import Galley.Intra.Util
import Galley.Types (UserClients, filterClients)
import Data.Id
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

import qualified Data.Set as Set

lookupClients :: [UserId] -> Galley UserClients
lookupClients uids = do
    (h, p) <- brigReq
    r <- call "brig"
        $ method POST . host h . port p
        . path "/i/clients"
        . json (UserSet $ Set.fromList uids)
        . expect2xx
    clients <- parseResponse (Error status502 "server-error") r
    return $ filterClients (not . Set.null) clients
