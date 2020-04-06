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

{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Client.API.Client
  ( registerClient,
    removeClient,
    updateClient,
    getUserPrekeys,
    getPrekey,
    getClients,
    module M,
  )
where

import Bilge
import Brig.Types.Client as M
import Data.ByteString.Conversion
import Data.Id
import Data.List.NonEmpty
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Session

registerClient :: MonadSession m => NewClient -> m M.Client
registerClient a = sessionRequest req rsc readBody
  where
    req =
      method POST
        . path "/clients"
        . acceptJson
        . json a
        $ empty
    rsc = status201 :| []

removeClient :: MonadSession m => ClientId -> RmClient -> m ()
removeClient cid r = sessionRequest req rsc (const $ return ())
  where
    req =
      method DELETE
        . paths ["clients", toByteString' cid]
        . acceptJson
        . json r
        $ empty
    rsc = status200 :| []

getClients :: MonadSession m => m [M.Client]
getClients = sessionRequest req rsc readBody
  where
    req =
      method GET
        . path "/clients"
        . acceptJson
        $ empty
    rsc = status200 :| []

updateClient :: MonadSession m => ClientId -> UpdateClient -> m ()
updateClient cid r = sessionRequest req rsc (const $ return ())
  where
    req =
      method PUT
        . paths ["clients", toByteString' cid]
        . acceptJson
        . json r
        $ empty
    rsc = status200 :| []

getUserPrekeys :: MonadSession m => UserId -> m PrekeyBundle
getUserPrekeys u = sessionRequest req rsc readBody
  where
    req =
      method GET
        . paths ["users", toByteString' u, "prekeys"]
        . acceptJson
        $ empty
    rsc = status200 :| []

getPrekey :: MonadSession m => UserId -> ClientId -> m ClientPrekey
getPrekey u c = sessionRequest req rsc readBody
  where
    req =
      method GET
        . paths ["users", toByteString' u, "prekeys", toByteString' c]
        . acceptJson
        $ empty
    rsc = status200 :| []
