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
import qualified Test.Federator.Client
import qualified Test.Federator.ExternalServer
import qualified Test.Federator.InternalServer
import qualified Test.Federator.Monitor
import qualified Test.Federator.Options
import qualified Test.Federator.Remote
import qualified Test.Federator.Response
import qualified Test.Federator.Validation
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Test.Federator.Options.tests,
        Test.Federator.Validation.tests,
        Test.Federator.Client.tests,
        Test.Federator.InternalServer.tests,
        Test.Federator.ExternalServer.tests,
        Test.Federator.Monitor.tests,
        Test.Federator.Remote.tests,
        Test.Federator.Response.tests
      ]
