{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module Test.Brig.Calling.Internal where

import Brig.Calling.Internal
import Data.Misc (mkHttpsUrl)
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import URI.ByteString.QQ as URI
import Wire.API.Call.Config (sftServer)

tests :: TestTree
tests =
  testGroup "Calling.API" $
    [ testGroup "sftServerFromSrvTarget" $
        [ testCase "when srvTarget ends with a dot" $ do
            let Right expectedServer = sftServer <$> mkHttpsUrl [URI.uri|https://sft1.env.example.com:9364|]
            assertEqual
              "the dot should be stripped from sft server"
              expectedServer
              (sftServerFromSrvTarget (1, 1, 9364, "sft1.env.example.com.")),
          testCase "when srvTarget doesn't end with a dot" $ do
            let Right expectedServer = sftServer <$> mkHttpsUrl [URI.uri|https://sft2.env.example.com:443|]
            assertEqual
              "there should still be no dot"
              expectedServer
              (sftServerFromSrvTarget (1, 1, 443, "sft2.env.example.com"))
        ]
    ]
