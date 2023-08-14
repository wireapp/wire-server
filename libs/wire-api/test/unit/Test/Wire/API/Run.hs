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

module Test.Wire.API.Run (main) where

import Imports
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Test.Wire.API.Call.Config qualified as Call.Config
import Test.Wire.API.Conversation qualified as Conversation
import Test.Wire.API.MLS qualified as MLS
import Test.Wire.API.MLS.Group qualified as Group
import Test.Wire.API.OAuth qualified as OAuth
import Test.Wire.API.RawJson qualified as RawJson
import Test.Wire.API.Roundtrip.Aeson qualified as Roundtrip.Aeson
import Test.Wire.API.Roundtrip.ByteString qualified as Roundtrip.ByteString
import Test.Wire.API.Roundtrip.CSV qualified as Roundtrip.CSV
import Test.Wire.API.Roundtrip.HttpApiData qualified as Roundtrip.HttpApiData
import Test.Wire.API.Roundtrip.MLS qualified as Roundtrip.MLS
import Test.Wire.API.Routes qualified as Routes
import Test.Wire.API.Routes.Version qualified as Routes.Version
import Test.Wire.API.Routes.Version.Wai qualified as Routes.Version.Wai
import Test.Wire.API.Swagger qualified as Swagger
import Test.Wire.API.Team.Export qualified as Team.Export
import Test.Wire.API.Team.Member qualified as Team.Member
import Test.Wire.API.User qualified as User
import Test.Wire.API.User.Auth qualified as User.Auth
import Test.Wire.API.User.RichInfo qualified as User.RichInfo
import Test.Wire.API.User.Search qualified as User.Search

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
        Group.tests,
        Routes.Version.tests,
        unsafePerformIO Routes.Version.Wai.tests,
        RawJson.tests,
        OAuth.tests
      ]
