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

module Test.Wire.API.Federation.API.BrigSpec where

import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.Types (parseEither)
import Imports
import Test.Hspec (Spec, describe, shouldBe, specify)
import Test.Wire.API.Federation.API.Util
import Wire.API.Federation.API.Brig (SearchRequest (..))

spec :: Spec
spec = describe "Wire.API.Federation.API.Brig" $ do
  describe "RoundTripTests" $ do
    jsonRoundTrip @SearchRequest
  describe "JSON Golden Tests" $ do
    jsonGoldenTest "SearchRequest" [aesonQQ|{"term": "searchedThing"}|] (SearchRequest "searchedThing" Nothing Nothing)

jsonGoldenTest :: (Eq a, Show a, FromJSON a) => String -> Value -> a -> Spec
jsonGoldenTest name val expected =
  specify ("GoldenTest: " <> show name) $ do
    parseEither parseJSON val `shouldBe` Right expected
