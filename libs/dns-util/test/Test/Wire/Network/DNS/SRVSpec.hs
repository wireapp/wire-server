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

module Test.Wire.Network.DNS.SRVSpec where

import Data.List.NonEmpty
import Imports
import qualified Network.DNS as DNS
import Test.Hspec
import Wire.Network.DNS.SRV

spec :: Spec
spec = describe "interpretResponse" $ do
  it "should interpret error correctly" $
    interpretResponse (Left DNS.UnknownDNSError) `shouldBe` SrvResponseError DNS.UnknownDNSError

  it "should interpret empty response as SrvNotAvailable" $
    interpretResponse (Right []) `shouldBe` SrvNotAvailable

  it "should interpret explicitly not available response as SrvNotAvailable" $
    interpretResponse (Right [(0, 0, 0, ".")]) `shouldBe` SrvNotAvailable

  it "should interpret an available service correctly" $ do
    let input =
          [ (0, 1, 443, "service01.example.com."),
            (10, 20, 8443, "service02.example.com.")
          ]
    let expectedOutput =
          SrvAvailable
            ( SrvEntry 0 1 (SrvTarget "service01.example.com." 443)
                :| [SrvEntry 10 20 (SrvTarget "service02.example.com." 8443)]
            )
    interpretResponse (Right input) `shouldBe` expectedOutput
