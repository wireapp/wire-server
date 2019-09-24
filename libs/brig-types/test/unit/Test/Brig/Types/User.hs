{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Brig.Types.User where

import Imports
import Brig.Types.Activation
import Brig.Types.User
import Data.Aeson
import Data.Aeson.Types
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "User" unitTests

unitTests :: [TestTree]
unitTests =
    [ let (=#=) :: Either String (Maybe UserIdentity) -> (Maybe UserSSOId, [Pair]) -> Assertion
          (=#=) uid (mssoid, object -> Object obj) = assertEqual "=#=" uid (parseEither (parseIdentity mssoid) obj)
          (=#=) _ bad = error $ "=#=: impossible: " <> show bad

      in testGroup "parseIdentity"
        [ testCase "FullIdentity" $
            Right (Just (FullIdentity hemail hphone)) =#= (Nothing, [email, phone])
        , testCase "EmailIdentity" $
            Right (Just (EmailIdentity hemail)) =#= (Nothing, [email])
        , testCase "PhoneIdentity" $
            Right (Just (PhoneIdentity hphone)) =#= (Nothing, [phone])
        , testCase "SSOIdentity" $ do
            Right (Just (SSOIdentity hssoid Nothing       Nothing))       =#= (Just hssoid, [ssoid])
            Right (Just (SSOIdentity hssoid Nothing       (Just hphone))) =#= (Just hssoid, [ssoid, phone])
            Right (Just (SSOIdentity hssoid (Just hemail) Nothing))       =#= (Just hssoid, [ssoid, email])
            Right (Just (SSOIdentity hssoid (Just hemail) (Just hphone))) =#= (Just hssoid, [ssoid, email, phone])
        , testCase "Bad phone" $
            Left "Error in $.phone: Invalid phone number. Expected E.164 format." =#= (Nothing, [badphone])
        , testCase "Bad email" $
            Left "Error in $.email: Invalid email. Expected '<local>@<domain>'." =#= (Nothing, [bademail])
        , testCase "Nothing" $
            Right Nothing =#= (Nothing, [("something_unrelated", "#")])
        ]
    ]
  where
    hemail    = Email "me" "example.com"
    email     = ("email", "me@example.com")
    bademail  = ("email", "justme")

    hphone    = Phone "+493012345678"
    phone     = ("phone", "+493012345678")
    badphone  = ("phone", "__@@")

    hssoid    = UserSSOId "nil" "nil"
    ssoid     = ("sso_id", toJSON hssoid)
