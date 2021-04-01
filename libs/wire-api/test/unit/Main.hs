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

module Main
  ( main,
  )
where

import Imports
import Test.Tasty
import qualified Test.Wire.API.Call.Config as Call.Config
import qualified Test.Wire.API.Roundtrip.Aeson as Roundtrip.Aeson
import qualified Test.Wire.API.Roundtrip.ByteString as Roundtrip.ByteString
import qualified Test.Wire.API.Roundtrip.CSV as Roundtrip.CSV
import qualified Test.Wire.API.Swagger as Swagger
import qualified Test.Wire.API.Team.Member as Team.Member
import qualified Test.Wire.API.User as User
import qualified Test.Wire.API.User.Search as User.Search
import qualified Test.Wire.API.User.RichInfo as User.RichInfo

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Call.Config.tests,
        Team.Member.tests,
        User.tests,
        User.Search.tests,
        User.RichInfo.tests,
        Roundtrip.Aeson.tests,
        Roundtrip.ByteString.tests,
        Swagger.tests,
        Roundtrip.CSV.tests
      ]
