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

module Test.Wire.API.User where

import Data.Aeson
import Data.Aeson.Types as Aeson
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.User (parseIdentity)
import Wire.API.User.Identity

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
