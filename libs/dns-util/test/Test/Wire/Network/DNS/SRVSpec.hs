{-# OPTIONS_GHC -Wno-x-partial #-}

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

module Test.Wire.Network.DNS.SRVSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Imports
import Network.DNS qualified as DNS
import Test.Hspec
import Wire.Network.DNS.SRV

spec :: Spec
spec = do
  describe "interpretResponse" $ do
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
  describe "orderSrvResult" $ do
    it "orders records according to ascending priority" $ do
      actual <-
        orderSrvResult . map toSrvEntry $
          [ -- priority, weight, port, domain
            (0, 0, 443, "offline.com"),
            (15, 10, 443, "main.com"),
            (0, 5, 443, "backup.com"),
            (2, 10, 443, "main.com"),
            (2, 20, 443, "main.com"),
            (3, 5, 443, "main.com"),
            (0, 0, 443, "backup.com")
          ]
      (srvPriority <$> actual) `shouldBe` [0, 0, 0, 2, 2, 3, 15]
    it "orders records with the same priority according to weight with certain probability" $ do
      let raw =
            map toSrvEntry $
              [ (2, 10, 443, "server1.com"),
                (2, 20, 443, "server2.com"),
                (2, 0, 443, "dontuseoften.com")
              ]
      -- order the list 50 times
      actuals <- replicateM 50 (orderSrvResult raw)
      let weightLists = fmap (fmap srvWeight) actuals
      let x = filter ((== 20) . head) weightLists
      let y = filter ((== 10) . head) weightLists
      -- we may have e.g.
      -- the server with weight 20 first in the list 30 times,
      -- the server with weight 10 15 times
      -- and the server with weight 0 5 times.
      -- We only check that there is *some* distribution
      length x `shouldSatisfy` (> 0)
      length x `shouldSatisfy` (< 49)
      length y `shouldSatisfy` (> 0)
      length y `shouldSatisfy` (< 49)
