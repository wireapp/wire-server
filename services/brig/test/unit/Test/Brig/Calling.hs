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

module Test.Brig.Calling where

import Brig.Calling
import Brig.Options
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Calling"
    [ testGroup
        "sftDomain"
        [ testCase "when service name is provided" $
            assertEqual
              "should use the service name to form domain"
              "_foo._tcp.example.com."
              (mkSFTDomain (SFTOptions "example.com" (Just "foo"))),
          testCase "when service name is not provided" $
            assertEqual
              "should assume service name to be 'sft'"
              "_sft._tcp.example.com."
              (mkSFTDomain (SFTOptions "example.com" Nothing))
        ]
    ]
