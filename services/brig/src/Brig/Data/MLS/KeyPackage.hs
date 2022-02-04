-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Data.MLS.KeyPackage
  ( insertKeyPackages,
  )
where

import Brig.App
import Cassandra
import Data.Id
import Data.Json.Util
import Imports
import Wire.API.MLS.KeyPackage

kpBlob :: KeyPackage -> Blob
kpBlob = Blob . fromBase64ByteString . kpData

insertKeyPackages :: UserId -> ClientId -> [KeyPackage] -> AppIO r ()
insertKeyPackages uid cid kps = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ kps $ \kp -> addPrepQuery q (uid, client cid, kpBlob kp)
  where
    q :: PrepQuery W (UserId, Text, Blob) ()
    q = "INSERT INTO mls_key_packages (uid, text, data) VALUES (?, ?, ?)"
