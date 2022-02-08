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
import Brig.Data.MLS.KeyPackage.Instances ()
import Cassandra
import Data.Id
import Imports
import Wire.API.MLS.KeyPackage

insertKeyPackages :: UserId -> ClientId -> [(KeyPackageRef, KeyPackageData)] -> AppIO r ()
insertKeyPackages uid cid kps = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ kps $ \(ref, kp) -> addPrepQuery q (uid, cid, ref, kp)
  where
    q :: PrepQuery W (UserId, ClientId, KeyPackageRef, KeyPackageData) ()
    q = "INSERT INTO mls_key_packages (uid, text, ref, data) VALUES (?, ?, ?, ?)"
