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

module Test.Data.Metrics.TypesSpec where

import Data.Metrics.Types
import Data.Tree
import Imports
import Test.Hspec

spec :: Spec
spec = describe "Data.Metrics.Types" $ do
  describe "treeLookup" $ do
    it "should match exact string when PathSegment is Right" $ do
      let testPaths = Paths [Node (Right "users") []]
      treeLookup testPaths ["users"] `shouldBe` Just "/users"

    it "should match anything when PathSegment is Left" $ do
      let testPaths = Paths [Node (Right "users") [Node (Left ":uid") []]]
      treeLookup testPaths ["users", "some-uuid"] `shouldBe` Just "/users/:uid"

    it "should match the correct endpoint when there are multiple Left options" $ do
      let testPaths =
            Paths
              [ Node
                  (Right "users")
                  [ Node (Left ":uid") [],
                    Node (Left ":domain") [Node (Left ":uid") []]
                  ]
              ]
      treeLookup testPaths ["users", "some-uuid"] `shouldBe` Just "/users/:uid"
      treeLookup testPaths ["users", "example.com", "some-uuid"] `shouldBe` Just "/users/:domain/:uid"

    -- See Note [Trees for Metrics]
    it "should match even when prefix of a path is matched" $ do
      let testPaths =
            Paths
              [ Node
                  (Right "users")
                  [ Node (Left ":uid") [],
                    Node (Left ":domain") [Node (Left ":uid") []]
                  ]
              ]
      treeLookup testPaths ["users"] `shouldBe` Just "/users"

    it "shouldn't match when endpoint doesn't fit" $ do
      let testPaths =
            Paths
              [ Node
                  (Right "users")
                  [ Node (Left ":uid") [],
                    Node (Left ":domain") [Node (Left ":uid") []]
                  ]
              ]
      treeLookup testPaths ["aliens"] `shouldBe` Nothing
      treeLookup testPaths ["users", "some-domain", "some-uuid", "extra-thing"] `shouldBe` Nothing

    it "should prioritize matches with most number of Rights" $ do
      let testPaths =
            Paths
              [ Node
                  (Right "users")
                  [ Node (Left ":domain") [Node (Left ":uid") []],
                    Node (Left ":uid") [Node (Right "clients") []]
                  ]
              ]
      let testPathsReverse =
            Paths
              [ Node
                  (Right "users")
                  [ Node (Left ":uid") [Node (Right "clients") []],
                    Node (Left ":domain") [Node (Left ":uid") []]
                  ]
              ]

      treeLookup testPaths ["users", "example.com", "some-uuid"] `shouldBe` Just "/users/:domain/:uid"
      treeLookup testPathsReverse ["users", "example.com", "some-uuid"] `shouldBe` Just "/users/:domain/:uid"

      treeLookup testPaths ["users", "some-uuid", "clients"] `shouldBe` Just "/users/:uid/clients"
      treeLookup testPathsReverse ["users", "some-uuid", "clients"] `shouldBe` Just "/users/:uid/clients"

    it "should prioritize matches with most number of Rights" $ do
      let testPaths =
            Paths
              [ Node
                  (Right "users")
                  [ Node (Left ":domain") [Node (Left ":uid") []],
                    Node (Left ":uid") [Node (Right "clients") []]
                  ]
              ]
      let testPathsReverse =
            Paths
              [ Node
                  (Right "users")
                  [ Node (Left ":uid") [Node (Right "clients") []],
                    Node (Left ":domain") [Node (Left ":uid") []]
                  ]
              ]

      treeLookup testPaths ["users", "example.com", "some-uuid"] `shouldBe` Just "/users/:domain/:uid"
      treeLookup testPathsReverse ["users", "example.com", "some-uuid"] `shouldBe` Just "/users/:domain/:uid"

      treeLookup testPaths ["users", "some-uuid", "clients"] `shouldBe` Just "/users/:uid/clients"
      treeLookup testPathsReverse ["users", "some-uuid", "clients"] `shouldBe` Just "/users/:uid/clients"
