{-# LANGUAGE DeriveAnyClass #-}

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

module Test.Galley.Roundtrip
  ( tests,
  )
where

import Data.Proxy (Proxy (Proxy))
import Imports
import Servant.Swagger (validateEveryToJSON)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import qualified Wire.API.Routes.Public.LegalHold as LegalHoldAPI

tests :: IO TestTree
tests = testSpec "Roundtrip" $ validateEveryToJSON (Proxy @LegalHoldAPI.ServantAPI)
