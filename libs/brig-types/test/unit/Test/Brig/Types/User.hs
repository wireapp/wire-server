{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Brig.Types.User where

import Brig.Types.Activation
import Brig.Types.Intra
import Brig.Types.Provider (UpdateServiceWhitelist)
import Brig.Types.Team.Invitation
import Brig.Types.Test.Arbitrary ()
import Brig.Types.User
import Data.Aeson
import Data.Aeson.QQ
import Data.Aeson.Types as Aeson
import qualified Data.Map as Map
import Data.Typeable (typeOf)
import Galley.Types.Teams
import Imports
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "User (types vs. aeson)" $ unitTests <> roundtripTests

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

roundtripTests :: [TestTree]
roundtripTests =
  [ run @BindingNewTeamUser,
    run @CheckHandles,
    run @CompletePasswordReset,
    run @DeleteUser,
    run @DeletionCodeTimeout,
    run @EmailRemove,
    run @EmailUpdate,
    run @HandleUpdate,
    run @InvitationList,
    run @Invitation,
    run @InvitationRequest,
    run @LocaleUpdate,
    run @NewPasswordReset,
    run @NewUser,
    run @PasswordChange,
    run @PhoneRemove,
    run @PhoneUpdate,
    run @ManagedByUpdate,
    run @ReAuthUser,
    run @SelfProfile,
    run @TeamMember,
    run @UpdateServiceWhitelist,
    run @UserHandleInfo,
    run @UserIdentity,
    run @UserProfile,
    run @User,
    run @RichInfo,
    run @UserUpdate,
    run @RichInfoUpdate,
    run @VerifyDeleteUser
  ]
  where
    run ::
      forall a.
      (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a) =>
      TestTree
    run = testProperty msg trip
      where
        msg = show $ typeOf (undefined :: a)
        trip (v :: a) =
          counterexample (show $ toJSON v) $
            Right v === (parseEither parseJSON . toJSON) v
