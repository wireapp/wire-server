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

import Aws.Arn qualified
import DelayQueue qualified
import Imports
import Json qualified
import Native qualified
import OpenSSL (withOpenSSL)
import ParseExistsError qualified
import Push qualified
import Test.Tasty
import ThreadBudget qualified

main :: IO ()
main =
  withOpenSSL . defaultMain $
    testGroup
      "Main"
      [ DelayQueue.tests,
        Json.tests,
        Native.tests,
        Push.tests,
        ThreadBudget.tests,
        ParseExistsError.tests,
        Aws.Arn.tests
      ]
