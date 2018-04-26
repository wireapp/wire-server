{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Brig.Types.User where

import Brig.Types.Activation
import Brig.Types.User
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Data.Typeable
import Test.Brig.Types.Arbitrary ()
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "User (types vs. aeson)" $ unitTests <> roundtripTests

unitTests :: [TestTree]
unitTests =
    [ let (=#=) :: Either String (Maybe UserIdentity) -> [Pair] -> Assertion
          (=#=) uid (object -> Object obj) = assertEqual "=#=" uid (parseEither parseIdentity obj)
          (=#=) _ bad = error $ "=#=: impossible: " <> show bad

      in testGroup "parseIdentity"
        [ testCase "FullIdentity" $
            Right (Just (FullIdentity hemail hphone)) =#= [email, phone]
        , testCase "EmailIdentity" $
            Right (Just (EmailIdentity hemail)) =#= [email]
        , testCase "PhoneIdentity" $
            Right (Just (PhoneIdentity hphone)) =#= [phone]
        , testCase "SSOIdentity" $ do
            Right (Just (SSOIdentity hssoid Nothing       Nothing))       =#= [ssoid]
            Right (Just (SSOIdentity hssoid Nothing       (Just hphone))) =#= [ssoid, phone]
            Right (Just (SSOIdentity hssoid (Just hemail) Nothing))       =#= [ssoid, email]
            Right (Just (SSOIdentity hssoid (Just hemail) (Just hphone))) =#= [ssoid, email, phone]
        , testCase "Bad phone" $
            Left "Error in $.phone: Invalid phone number. Expected E.164 format." =#= [badphone]
        , testCase "Bad email" $
            Left "Error in $.email: Invalid email. Expected '<local>@<domain>'." =#= [bademail]
        , testCase "Nothing" $
            Right Nothing =#= [("something_unrelated", "#")]
        ]

    , let (=#=) :: Either String (MaybeUserIdentity Bool) -> [Pair] -> Assertion
          (=#=) uid (object -> Object obj) = assertEqual "=#=" uid (parseEither (parseMaybeIdentity "expire") obj)
          (=#=) _ bad = error $ "=#=: impossible: " <> show bad

          expire    = ("expire", Bool True)
          badexpire = ("expire", "#23")

      in testGroup "parseMaybeIdentity"
        [ testCase "UserIdentity" $
            Right (JustUserIdentity (PhoneIdentity hphone)) =#= [phone]
        , testCase "ephemeral" $
            Right (NothingUserIdentity True) =#= [expire]
        , testCase "errors" $ do
            Left "Error in $.expire: expected Bool, encountered String"
              =#= [badexpire]
            Left "Error in $: parseMaybeIdentityUser: (Just (PhoneIdentity (Phone {fromPhone = \"+493012345678\"})),Just True)"
              =#= [phone, expire]
            Left "Error in $: parseMaybeIdentityUser: (Nothing,Nothing)"
              =#= []
        ]
    ]
  where
    hemail    = Email "me" "example.com"
    email     = ("email", "me@example.com")
    bademail  = ("email", "justme")

    hphone    = Phone "+493012345678"
    phone     = ("phone", "+493012345678")
    badphone  = ("phone", "__@@")

    hssoid    = UserSSOId "blu:bnee"
    ssoid     = ("ssoid", "blu:bnee")


roundtripTests :: [TestTree]
roundtripTests =
    [ run @BindingNewTeamUser Proxy
    , run @CheckHandles Proxy
    , run @CompletePasswordReset Proxy
    , run @DeleteUser Proxy
    , run @DeletionCodeTimeout Proxy
    , run @EmailRemove Proxy
    , run @EmailUpdate Proxy
    , run @HandleUpdate Proxy
    , run @LocaleUpdate Proxy
    , run @NewPasswordReset Proxy
    , run @UserIdentity Proxy
    , run @NewUser Proxy
    , run @PasswordChange Proxy
    , run @PhoneRemove Proxy
    , run @PhoneUpdate Proxy
    , run @SelfProfile Proxy
    , run @UserHandleInfo Proxy
    , run @UserProfile Proxy
    , run @UserUpdate Proxy
    , run @User Proxy
    , run @VerifyDeleteUser Proxy
    ]
  where
    run :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a)
        => Proxy a -> TestTree
    run Proxy = testProperty msg trip
      where
        msg = show $ typeOf (undefined :: a)
        trip (v :: a) = counterexample (show $ toJSON v)
                      $ Right v === (parseEither parseJSON . toJSON) v
