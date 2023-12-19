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
import API.BrigInternal
import API.Common
import SetupHelpers
import Testlib.Prelude

testUpdateServiceUpdateAcceptHeader :: HasCallStack => App ()
testUpdateServiceUpdateAcceptHeader = do
  let dom = OwnDomain
  email <- randomEmail
  alice <- randomUser dom def
  provider <- newProvider alice def {newProviderEmail = email}
  providerId <- provider %. "id" & asString
  pass <- provider %. "password" & asString
  (key, code) <- do
    pair <-
      getProviderActivationCodeInternal dom email `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json
    k <- pair %. "key" & asString
    c <- pair %. "code" & asString
    pure (k, c)
  activateProvider dom key code
  void $ loginProvider dom email pass
  service <- newService dom providerId def
  serviceId <- service %. "id"
  void $
    updateService
      dom
      providerId
      serviceId
      (Just "application/json")
      (Just "brand new service")
      >>= getBody 200
