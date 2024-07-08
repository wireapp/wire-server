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
import OpenSSL (withOpenSSL)
import Test.Federator.Client qualified
import Test.Federator.ExternalServer qualified
import Test.Federator.InternalServer qualified
import Test.Federator.Monitor qualified
import Test.Federator.Options qualified
import Test.Federator.Remote qualified
import Test.Federator.Validation qualified
import Test.Tasty

main :: IO ()
main =
  withOpenSSL $
    defaultMain $
      testGroup
        "Tests"
        [ Test.Federator.Options.tests,
          Test.Federator.Validation.tests,
          Test.Federator.Client.tests,
          Test.Federator.InternalServer.tests,
          Test.Federator.ExternalServer.tests,
          Test.Federator.Monitor.tests,
          Test.Federator.Remote.tests
        ]
