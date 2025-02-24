-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Services where

import API.Brig
import SetupHelpers
import Testlib.Prelude

testUpdateServiceUpdateAcceptHeader :: (HasCallStack) => App ()
testUpdateServiceUpdateAcceptHeader = do
  let dom = OwnDomain
  alice <- randomUser dom def
  provider <- setupProvider alice def
  pId <- provider %. "id" & asString
  service <- newService dom pId def
  sId <- service %. "id"
  void
    $ updateService dom pId sId (Just "application/json") (Just "brand new service")
    >>= getBody 200
  void
    $ updateService dom pId sId (Just "text/plain") (Just "even newer service")
    >>= getBody 200
  void
    $ updateService dom pId sId Nothing (Just "really old service")
    >>= getBody 200
