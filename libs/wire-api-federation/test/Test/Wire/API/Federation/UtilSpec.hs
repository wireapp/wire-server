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

module Test.Wire.API.Federation.UtilSpec where

import Data.Aeson
import Data.SOP (I (..))
import Imports
import Servant.API (WithStatus (..), inject)
import Test.Hspec
import Wire.API.Federation.Util.Aeson (CustomEncodedUnion (..))

type ExampleMembers = [WithStatus 200 Int, WithStatus 400 String]

type ExampleUnion = CustomEncodedUnion ExampleMembers

example1 :: ExampleUnion
example1 = CustomEncodedUnion (inject (I (WithStatus @200 (42 :: Int))))

example2 :: ExampleUnion
example2 = CustomEncodedUnion (inject (I (WithStatus @400 ("failed" :: String))))

spec :: Spec
spec = describe "CustomEncodedUnion" $ do
  it "should generate a JSON object with status and value fields" $ do
    toJSON example1 `shouldBe` object ["status" .= (200 :: Int), "value" .= (42 :: Int)]
    toJSON example2 `shouldBe` object ["status" .= (400 :: Int), "value" .= ("failed" :: String)]

  it "should parse a JSON object containing status and value fields" $ do
    fromJSON (object ["status" .= (200 :: Int), "value" .= (42 :: Int)])
      `shouldBe` Success example1
    fromJSON (object ["status" .= (400 :: Int), "value" .= ("failed" :: String)])
      `shouldBe` Success example2

  it "should give an error when the status code is not in the union" $ do
    fromJSON (object ["status" .= (201 :: Int), "value" .= (37 :: Int)])
      `shouldBe` (Error "unexpected status 201" :: Result ExampleUnion)

  it "should give an error for invalid JSON" $ do
    fromJSON (object ["value" .= True])
      `shouldBe` (Error "key \"status\" not found" :: Result ExampleUnion)
    fromJSON (object ["status" .= (200 :: Int)])
      `shouldBe` (Error "key \"value\" not found" :: Result ExampleUnion)
    fromJSON (object ["status" .= (200 :: Int), "value" .= ("42" :: String)])
      `shouldBe` (Error "Error in $: parsing Int failed, expected Number, but encountered String" :: Result ExampleUnion)
