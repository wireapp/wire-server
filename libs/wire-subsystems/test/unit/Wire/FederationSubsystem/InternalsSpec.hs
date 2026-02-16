-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foun(Domain "a.com")tion, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.FederationSubsystem.InternalsSpec (spec) where

import Data.Domain
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Test.Hspec
import Wire.API.Federation.API.Brig
import Wire.API.FederationStatus
import Wire.FederationSubsystem.Internals (firstMissingConnectionOrFullyConnected)

spec :: Spec
spec =
  describe "firstMissingConnectionOrFullyConnected" $ do
    let mkResponse :: Domain -> [Domain] -> Remote NonConnectedBackends
        mkResponse d = toRemoteUnsafe d . NonConnectedBackends . Set.fromList
    it "empty" $
      firstMissingConnectionOrFullyConnected [] `shouldBe` FullyConnected
    it "single response" $
      firstMissingConnectionOrFullyConnected [mkResponse (Domain "a.com") []] `shouldBe` FullyConnected
    it "multiple responses" $
      firstMissingConnectionOrFullyConnected [mkResponse (Domain "a.com") [], mkResponse (Domain "b.com") []] `shouldBe` FullyConnected
    it "single bad responses" $
      firstMissingConnectionOrFullyConnected [mkResponse (Domain "a.com") [Domain "b.com"]] `shouldBe` NotConnectedDomains (Domain "a.com") (Domain "b.com")
    it "one good one bad response" $
      firstMissingConnectionOrFullyConnected [mkResponse (Domain "a.com") [], mkResponse (Domain "b.com") [Domain "c.com"]] `shouldBe` NotConnectedDomains (Domain "b.com") (Domain "c.com")
    it "one bad one good response" $
      firstMissingConnectionOrFullyConnected [mkResponse (Domain "b.com") [Domain "c.com"], mkResponse (Domain "a.com") []] `shouldBe` NotConnectedDomains (Domain "b.com") (Domain "c.com")
    it "one bad multiple good responses" $
      firstMissingConnectionOrFullyConnected [mkResponse (Domain "b.com") [Domain "c.com"], mkResponse (Domain "a.com") [], mkResponse (Domain "d.com") []] `shouldBe` NotConnectedDomains (Domain "b.com") (Domain "c.com")
    it "multiple bad responses" $
      firstMissingConnectionOrFullyConnected [mkResponse (Domain "a.com") [Domain "b.com"], mkResponse (Domain "b.com") [Domain "a.com"]] `shouldBe` NotConnectedDomains (Domain "a.com") (Domain "b.com")
