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
import Imports
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Web.Scim.Schema.Common as Scim
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
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfoMapAndList mempty) $ fromJSON inputJSON,
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
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfoMapAndList [RichField "foo" "bar"]) $ fromJSON inputJSON,
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
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfoMapAndList [RichField "foo" "bar"]) $ fromJSON inputJSON,
        testCase "RichInfoMapAndList as only assoc list" $ do
          let inputJSON =
                [aesonQQ|{
                                     "urn:wire:scim:schemas:profile:1.0" : {
                                        "richinfo": [{"type": "foo", "value": "bar"}]
                                     }
                                   }|]
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfoMapAndList [RichField "foo" "bar"]) $ fromJSON inputJSON,
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
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfoMapAndList [RichField "foo" "bar", RichField "bar" "baz"]) $ fromJSON inputJSON,
        testCase "Without Old RichInfoMapAndList" $ do
          let inputJSON =
                [aesonQQ|{
                                    "urn:ietf:params:scim:schemas:extension:wire:1.0:User" : {
                                    }
                                  }|]
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfoMapAndList []) $ fromJSON inputJSON,
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
          assertEqual "RichInfoMapAndList" Nothing $ parseMaybe (parseJSON @RichInfoMapAndList) inputJSON,
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
          assertEqual "RichInfoMapAndList" (Aeson.Success $ mkRichInfoMapAndList [RichField "foo" "bar"]) $ fromJSON inputJSON
      ]
  ]
    <> moreRichInfoNormalizationTests

moreRichInfoNormalizationTests :: [TestTree]
moreRichInfoNormalizationTests =
  [ testCase "'normalizeRichInfoAssocList'" $ do
      let f = length . unRichInfoAssocList
      assertEqual mempty (f (normalizeRichInfoAssocList assocs)) (f assocs)
      assertEqual mempty (normalizeRichInfoAssocList assocs) assocs,
    testGroup
      "'toRichInfoAssocList', 'fromRichInfoAssocList'"
      [ testCase "works (counter-example of earlier bug)" $ do
          let x = mkRichInfoMapAndList [RichField "A" "b", RichField "a" "x"]
              y = (fromRichInfoAssocList . toRichInfoAssocList) x
          assertEqual mempty (toRichInfoAssocList x) (toRichInfoAssocList y),
        testCase "works (counter-example of earlier bug)" $ do
          assertEqual mempty (jsonroundtrip assocs) assocs
          assertEqual mempty (toRichInfoAssocList . fromRichInfoAssocList $ assocs) assocs
          assertEqual mempty (toRichInfoAssocList . jsonroundtrip . fromRichInfoAssocList $ assocs) assocs,
        testProperty "works (property)" $ \(someAssocs :: RichInfoAssocList) ->
          (jsonroundtrip someAssocs) === someAssocs
            .&&. (toRichInfoAssocList . fromRichInfoAssocList $ someAssocs) === someAssocs
            .&&. (toRichInfoAssocList . jsonroundtrip . fromRichInfoAssocList $ someAssocs) === someAssocs
      ]
  ]
  where
    jsonroundtrip :: forall a. (ToJSON a, FromJSON a) => a -> a
    jsonroundtrip = unsafeParse . Scim.jsonLower . Aeson.toJSON
      where
        unsafeParse = either (error . show) id . Aeson.parseEither Aeson.parseJSON

    assocs :: RichInfoAssocList
    assocs = mkRichInfoAssocList [RichField {richFieldType = "0-plIe\176041Sdu]\129492ouXy*]j\49123`jDNJ:N%\32939\&6\183443\\>HSi\6502q,\28951wZ].\11331w`", richFieldValue = "C ny6Nx0f&b\121034\29092r"}, RichField {richFieldType = "[&c;VP9\42304Q.I\43963OS\83057}G ]\175364xYLqO\156677q*ZBtZ`vKc", richFieldValue = "+FEv\28180"}, RichField {richFieldType = "}121@^z{", richFieldValue = "{KZQqjqs Py%ETB>;y1}\142167\181794\164475p"}, RichField {richFieldType = "\48098\&2#-p\68080\&9\37971|\190007K|m(", richFieldValue = ":j7\83424lQ\19571\188281*[)D8\50056\9019n\189416\100233]*!={FX|/!!&my]+8\175071\135759\&0\13316K'(\14120\172092w,2"}, RichField {richFieldType = "\50520MX>\\kQcBz\169538\147873\\\177286FqS!GW]#\20027_n", richFieldValue = "53\190108.?%t[ &9=hd9t:}Q@yj#w~B\164946B# fs!\39091}eEP"}, RichField {richFieldType = "sE7hmj\164437:", richFieldValue = "ns\"EJftf6~g5U\"&tt\20456@]M"}, RichField {richFieldType = "\172698p\41097sHk \37897X0Io\8286OU\173780\18370h\46873&GAOpuQU+T)]rC\5068WCA\68875(-\175596'", richFieldValue = "lRiP"}]
