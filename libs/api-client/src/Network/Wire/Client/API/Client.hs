{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Client.API.Client
    ( registerClient
    , removeClient
    , updateClient
    , getUserPrekeys
    , getPrekey
    , getClients
    , module M
    ) where

import Imports
import Bilge
import Brig.Types.Client as M
import Data.ByteString.Conversion
import Data.Id
import Data.List.NonEmpty
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Session

registerClient :: MonadSession m => NewClient protected -> m M.Client
registerClient a = sessionRequest req rsc readBody
  where
    req = method POST
        . path "/clients"
        . acceptJson
        . json a
        $ empty
    rsc = status201 :| []

removeClient :: MonadSession m => ClientId -> RmClient protected -> m ()
removeClient cid r = sessionRequest req rsc (const $ return ())
  where
    req = method DELETE
        . paths ["clients", toByteString' cid]
        . acceptJson
        . json r
        $ empty
    rsc = status200 :| []

getClients :: MonadSession m => m [M.Client]
getClients = sessionRequest req rsc readBody
  where
    req = method GET
        . path "/clients"
        . acceptJson
        $ empty
    rsc = status200 :| []

updateClient :: MonadSession m => ClientId -> UpdateClient -> m ()
updateClient cid r = sessionRequest req rsc (const $ return ())
  where
    req = method PUT
        . paths ["clients", toByteString' cid]
        . acceptJson
        . json r
        $ empty
    rsc = status200 :| []

getUserPrekeys :: MonadSession m => UserId -> m PrekeyBundle
getUserPrekeys u = sessionRequest req rsc readBody
  where
    req = method GET
        . paths ["users", toByteString' u, "prekeys"]
        . acceptJson
        $ empty
    rsc = status200 :| []

getPrekey :: MonadSession m => UserId -> ClientId -> m ClientPrekey
getPrekey u c = sessionRequest req rsc readBody
  where
    req = method GET
        . paths ["users", toByteString' u, "prekeys", toByteString' c]
        . acceptJson
        $ empty
    rsc = status200 :| []
