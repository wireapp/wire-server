{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

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

module Test.Wire.API.User where

import Data.Aeson
import Data.Aeson.QQ
import Data.Aeson.Types as Aeson
import qualified Data.Map as Map
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.User (parseIdentity)
import Wire.API.User.Identity
import Wire.API.User.RichInfo

tests :: TestTree
tests = testGroup "User (types vs. aeson)" $ unitTests

unitTests :: [TestTree]
unitTests =
  [ let (=#=) :: Either String (Maybe UserIdentity) -> (Maybe UserSSOId, [Pair]) -> Assertion
        (=#=) uid (mssoid, object -> Object obj) = assertEqual "=#=" uid (parseEither (parseIdentity mssoid) obj)
        (=#=) _ bad = error $ "=#=: impossible: " <> show bad
     in testGroup
          "parseIdentity"
          [ testCase "FullIdentity" $
              Right (Just (FullIdentity hemail hphone)) =#= (Nothing, [email, phone]),
            testCase "EmailIdentity" $
              Right (Just (EmailIdentity hemail)) =#= (Nothing, [email]),
            testCase "PhoneIdentity" $
              Right (Just (PhoneIdentity hphone)) =#= (Nothing, [phone]),
            testCase "SSOIdentity" $ do
              Right (Just (SSOIdentity hssoid Nothing Nothing)) =#= (Just hssoid, [ssoid])
              Right (Just (SSOIdentity hssoid Nothing (Just hphone))) =#= (Just hssoid, [ssoid, phone])
              Right (Just (SSOIdentity hssoid (Just hemail) Nothing)) =#= (Just hssoid, [ssoid, email])
              Right (Just (SSOIdentity hssoid (Just hemail) (Just hphone))) =#= (Just hssoid, [ssoid, email, phone]),
            testCase "Bad phone" $
              Left "Error in $.phone: Invalid phone number. Expected E.164 format." =#= (Nothing, [badphone]),
            testCase "Bad email" $
              Left "Error in $.email: Invalid email. Expected '<local>@<domain>'." =#= (Nothing, [bademail]),
            testCase "Nothing" $
              Right Nothing =#= (Nothing, [("something_unrelated", "#")])
          ],
    let check msg ri rial = testCase msg $ assertEqual "failed" (toRichInfoAssocList ri) rial
     in testGroup
          "RichInfo to RichInfoAssocList"
          [ check
              "map comes in alpha order, prepended to the assoc list"
              (RichInfo (Map.fromList [("c", "3"), ("a", "1")]) [RichField "b" "2"])
              (RichInfoAssocList [RichField "a" "1", RichField "c" "3", RichField "b" "2"]),
            check
              "map overwrites assoc list"
              (RichInfo (Map.singleton "a" "b") [RichField "a" "c"])
              (RichInfoAssocList [RichField "a" "b"]),
            check
              "treats RichField keys case-insensitively"
              (RichInfo (Map.singleton "a" "b") [RichField "A" "c", RichField "B" "b"])
              (RichInfoAssocList [RichField "a" "b", RichField "B" "b"])
          ],
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
  where
    hemail = Email "me" "example.com"
    email = ("email", "me@example.com")
    bademail = ("email", "justme")
    hphone = Phone "+493012345678"
    phone = ("phone", "+493012345678")
    badphone = ("phone", "__@@")
    hssoid = UserSSOId "nil" "nil"
    ssoid = ("sso_id", toJSON hssoid)
