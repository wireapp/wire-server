{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Test.Wire.API.User.RichInfo where

import Data.Aeson
import Data.Aeson.QQ
import Data.Aeson.Types as Aeson
import qualified Data.Map as Map
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Wire.API.User.RichInfo

tests :: TestTree
tests =
  testGroup "User (types vs. aeson)" testRichInfo

testRichInfo :: [TestTree]
testRichInfo =
  [ let check msg ri rial = testCase msg $ assertEqual "failed" rial (toRichInfoAssocList ri)
     in testGroup
          "RichInfo to RichInfoAssocList"
          [ check
              "map comes in alpha order, at the end of the assoc list"
              (RichInfo (Map.fromList [("c", "3"), ("a", "1")]) [RichField "b" "2"])
              (RichInfoAssocList [RichField "b" "2", RichField "a" "1", RichField "c" "3"]),
            check
              "map overwrites assoc list"
              (RichInfo (Map.singleton "a" "b") [RichField "a" "c"])
              (RichInfoAssocList [RichField "a" "b"]),
            check
              "treats RichField keys case-insensitively"
              (RichInfo (Map.singleton "a" "b") [RichField "A" "c", RichField "B" "b"])
              (RichInfoAssocList [RichField "a" "b", RichField "B" "b"])
          ],
    testProperty "RichInfoAssocList <-> RichInfo roundtrip" $ \riAssocList ->
      toRichInfoAssocList (fromRichInfoAssocList riAssocList) === riAssocList,
    testGroup
      "RichInfo Examples"
      [ testCase "Empty rich info" $ do
          let inputJSON =
                [aesonQQ|{
                                     "urn:ietf:params:scim:schemas:extension:wire:1.0:User" : {},
                                     "urn:wire:scim:schemas:profile:1.0" : {
                                        "richinfo": {
                                          "fields" : [],
                                          "version" : 0
                                        }
                                     }
                                   }|]
          assertEqual "RichInfo" (Aeson.Success $ RichInfo mempty mempty) $ fromJSON inputJSON,
        testCase "Old RichInfo" $ do
          let inputJSON =
                [aesonQQ|{
                                     "urn:wire:scim:schemas:profile:1.0" : {
                                        "richinfo": {
                                          "fields" : [{"type": "foo", "value": "bar"}],
                                          "version" : 0
                                        }
                                     }
                                   }|]
          assertEqual "RichInfo" (Aeson.Success $ RichInfo mempty [RichField "foo" "bar"]) $ fromJSON inputJSON,
        testCase "case insensitive 'richinfo'" $ do
          let inputJSON =
                [aesonQQ|{
                                     "urn:wire:scim:schemas:profile:1.0" : {
                                        "richINFO": {
                                          "fields" : [{"type": "foo", "value": "bar"}],
                                          "version" : 0
                                        }
                                     }
                                   }|]
          assertEqual "RichInfo" (Aeson.Success $ RichInfo mempty [RichField "foo" "bar"]) $ fromJSON inputJSON,
        testCase "RichInfo as only assoc list" $ do
          let inputJSON =
                [aesonQQ|{
                                     "urn:wire:scim:schemas:profile:1.0" : {
                                        "richinfo": [{"type": "foo", "value": "bar"}]
                                     }
                                   }|]
          assertEqual "RichInfo" (Aeson.Success $ RichInfo mempty [RichField "foo" "bar"]) $ fromJSON inputJSON,
        testCase "RichInfo Map" $ do
          let inputJSON =
                [aesonQQ|{
                                     "urn:ietf:params:scim:schemas:extension:wire:1.0:User" : {
                                       "bar": "baz"
                                     },
                                     "urn:wire:scim:schemas:profile:1.0" : {
                                        "richinfo" : {
                                          "fields" : [{"type" : "foo", "value" : "bar"}],
                                          "version" : 0
                                        }
                                     }
                                   }|]
          assertEqual "RichInfo" (Aeson.Success $ RichInfo (Map.singleton "bar" "baz") [RichField "foo" "bar"]) $ fromJSON inputJSON,
        testCase "Without Old RichInfo" $ do
          let inputJSON =
                [aesonQQ|{
                                    "urn:ietf:params:scim:schemas:extension:wire:1.0:User" : {
                                      "bar": "baz"
                                    }
                                  }|]
          assertEqual "RichInfo" (Aeson.Success $ RichInfo (Map.singleton "bar" "baz") []) $ fromJSON inputJSON,
        testCase "wrong version" $ do
          let inputJSON =
                [aesonQQ|{
                                     "urn:wire:scim:schemas:profile:1.0" : {
                                        "richinfo" : {
                                          "fields" : [{"type" : "foo", "value" : "bar"}],
                                          "version" : 42
                                        }
                                     }
                                  }|]
          assertEqual "RichInfo" Nothing $ parseMaybe (parseJSON @RichInfo) inputJSON,
        testCase "drop empty fields" $ do
          let inputJSON =
                [aesonQQ|{
                                     "urn:wire:scim:schemas:profile:1.0" : {
                                        "richinfo" : {
                                          "fields" : [
                                            {"type" : "foo", "value" : "bar"},
                                            {"type" : "dropped", "value" : ""}
                                          ],
                                          "version" : 0
                                        }
                                     }
                                   }|]
          assertEqual "RichInfo" (Aeson.Success $ RichInfo mempty [RichField "foo" "bar"]) $ fromJSON inputJSON
      ]
  ]
