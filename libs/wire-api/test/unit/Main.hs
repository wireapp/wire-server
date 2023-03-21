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

module Main
  ( main,
  )
where

import Imports
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import qualified Test.Wire.API.Call.Config as Call.Config
import qualified Test.Wire.API.Conversation as Conversation
import qualified Test.Wire.API.MLS as MLS
import qualified Test.Wire.API.RawJson as RawJson
import qualified Test.Wire.API.Roundtrip.Aeson as Roundtrip.Aeson
import qualified Test.Wire.API.Roundtrip.ByteString as Roundtrip.ByteString
import qualified Test.Wire.API.Roundtrip.CSV as Roundtrip.CSV
import qualified Test.Wire.API.Roundtrip.HttpApiData as Roundtrip.HttpApiData
import qualified Test.Wire.API.Roundtrip.MLS as Roundtrip.MLS
import qualified Test.Wire.API.Routes as Routes
import qualified Test.Wire.API.Routes.Version as Routes.Version
import qualified Test.Wire.API.Routes.Version.Wai as Routes.Version.Wai
import qualified Test.Wire.API.Swagger as Swagger
import qualified Test.Wire.API.Team.Export as Team.Export
import qualified Test.Wire.API.Team.Member as Team.Member
import qualified Test.Wire.API.User as User
import qualified Test.Wire.API.User.Auth as User.Auth
import qualified Test.Wire.API.User.RichInfo as User.RichInfo
import qualified Test.Wire.API.User.Search as User.Search

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Call.Config.tests,
        Team.Member.tests,
        Team.Export.tests,
        User.tests,
        User.Search.tests,
        User.RichInfo.tests,
        User.Auth.tests,
        Roundtrip.Aeson.tests,
        Roundtrip.ByteString.tests,
        Roundtrip.HttpApiData.tests,
        Roundtrip.MLS.tests,
        Swagger.tests,
        Roundtrip.CSV.tests,
        Routes.tests,
        Conversation.tests,
        MLS.tests,
        Routes.Version.tests,
        unsafePerformIO Routes.Version.Wai.tests,
        RawJson.tests
      ]
