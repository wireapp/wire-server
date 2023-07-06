{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Test.Wire.API.User.RichInfo where

import Data.Aeson
import Data.Aeson.QQ
import Data.Aeson.Types as Aeson
import Imports
import Test.QuickCheck
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
          "RichInfoMapAndList to RichInfoAssocList"
          [ check
              "map comes in alpha order, at the end of the assoc list"
              (mkRichInfoMapAndList [RichField "b" "2", RichField "c" "3", RichField "a" "1"])
              (mkRichInfoAssocList [RichField "b" "2", RichField "c" "3", RichField "a" "1"]),
            check
              "map overwrites assoc list"
              (mkRichInfoMapAndList [RichField "a" "c"])
              (mkRichInfoAssocList [RichField "a" "c"]),
            check
              "treats RichField keys case-insensitively"
              (mkRichInfoMapAndList [RichField "A" "c", RichField "B" "b"])
              (mkRichInfoAssocList [RichField "a" "c", RichField "b" "b"])
          ],
    testProperty "RichInfoAssocList <-> RichInfoMapAndList roundtrip" $ \riAssocList -> do
      toRichInfoAssocList (fromRichInfoAssocList riAssocList) === riAssocList,
    testProperty "assoc list in RichInfoMapAndList is stable and forms a prefix of the toRichInfoAssocList value" $ \ri -> do
      let keys = fmap (\(RichField k _) -> k)
      keys (richInfoAssocList ri) `isPrefixOf` keys (unRichInfoAssocList (toRichInfoAssocList ri)),
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
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfo mempty) $ fromJSON inputJSON,
        testCase "Old RichInfoMapAndList" $ do
          let inputJSON =
                [aesonQQ|{
                                     "urn:wire:scim:schemas:profile:1.0" : {
                                        "richinfo": {
                                          "fields" : [{"type": "foo", "value": "bar"}],
                                          "version" : 0
                                        }
                                     }
                                   }|]
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfo [RichField "foo" "bar"]) $ fromJSON inputJSON,
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
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfo [RichField "foo" "bar"]) $ fromJSON inputJSON,
        testCase "RichInfoMapAndList as only assoc list" $ do
          let inputJSON =
                [aesonQQ|{
                                     "urn:wire:scim:schemas:profile:1.0" : {
                                        "richinfo": [{"type": "foo", "value": "bar"}]
                                     }
                                   }|]
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfo [RichField "foo" "bar"]) $ fromJSON inputJSON,
        testCase "RichInfoMapAndList Map" $ do
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
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfo [RichField "foo" "bar", RichField "bar" "baz"]) $ fromJSON inputJSON,
        testCase "Without Old RichInfoMapAndList" $ do
          let inputJSON =
                [aesonQQ|{
                                    "urn:ietf:params:scim:schemas:extension:wire:1.0:User" : {
                                    }
                                  }|]
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfo []) $ fromJSON inputJSON,
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
          assertEqual "RichInfoMapAndList" Nothing $ parseMaybe (parseJSON @RichInfo) inputJSON,
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
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfo [RichField "foo" "bar"]) $ fromJSON inputJSON
      ]
  ]
    <> moreRichInfoNormalizationTests

moreRichInfoNormalizationTests :: [TestTree]
moreRichInfoNormalizationTests =
  [ testGroup
      "'toRichInfoAssocList', 'fromRichInfoAssocList'"
      [ testCase "works (counter-example of earlier bug)" $ do
          let x = mkRichInfoMapAndList [RichField "A" "b", RichField "a" "x"]
              y = (fromRichInfoAssocList . toRichInfoAssocList) x
          assertEqual mempty (toRichInfoAssocList x) (toRichInfoAssocList y),
        testProperty "works (property)" $ \(someAssocs :: RichInfoAssocList) ->
          toRichInfoAssocList (fromRichInfoAssocList someAssocs) === someAssocs
      ]
  ]
