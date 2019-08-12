{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is where currently all the json roundtrip tests happen for brig-types and
-- galley-types.
module Test.Brig.Types.Common where

import Imports

import Brig.Types.Common
import Brig.Types.Team.LegalHold
import Brig.Types.Test.Arbitrary ()
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Typeable (typeOf)
import Galley.Types.Teams
import Galley.Types.Teams.SSO
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


-- NB: validateEveryToJSON from servant-swagger doesn't render these tests unnecessary!

tests :: TestTree
tests = testGroup "Common (types vs. aeson)"
    [ run @Handle Proxy
    , run @Name Proxy
    , run @ColourId Proxy
    , run @Email Proxy
    , run @Phone Proxy
    , run @UserIdentity Proxy
    , run @UserSSOId Proxy
    , run @AssetSize Proxy
    , run @Asset Proxy
    , run @ExcludedPrefix Proxy
    , run @ManagedBy Proxy
    , run @TeamMemberDeleteData Proxy
    , run @LegalHoldStatus Proxy
    , run @LegalHoldTeamConfig Proxy
    , run @NewLegalHoldService Proxy
    , run @LegalHoldService Proxy
    , run @ViewLegalHoldService Proxy
    , run @NewLegalHoldClient Proxy
    , run @RequestNewLegalHoldClient Proxy
    , run @UserLegalHoldStatusResponse Proxy
    , run @LegalHoldServiceConfirm Proxy
    , run @LegalHoldClientRequest Proxy
    , run @RemoveLegalHoldSettingsRequest Proxy
    , run @DisableLegalHoldForUserRequest Proxy
    , run @ApproveLegalHoldForUserRequest Proxy
    , run @SSOStatus Proxy
    , run @SSOTeamConfig Proxy
    , testCase "{} is a valid TeamMemberDeleteData" $ do
        assertEqual "{}" (Right $ newTeamMemberDeleteData Nothing) (eitherDecode "{}")
    ]
  where
    run :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a)
         => Proxy a -> TestTree
    run Proxy = testProperty msg trip
      where
        msg = show $ typeOf (undefined :: a)
        trip (v :: a) = counterexample (show $ toJSON v)
                      $ Right v === (parseEither parseJSON . toJSON) v


instance Arbitrary TeamMemberDeleteData where
  arbitrary = newTeamMemberDeleteData <$> arbitrary

instance Eq TeamMemberDeleteData where
  a == b = a ^. tmdAuthPassword == b ^. tmdAuthPassword

instance Show TeamMemberDeleteData where
  show a = "(TeamMemberDeleteData " <> show (a ^. tmdAuthPassword) <> ")"

instance Arbitrary SSOStatus where
  arbitrary = Test.Tasty.QuickCheck.elements [minBound..]

instance Arbitrary SSOTeamConfig where
  arbitrary = SSOTeamConfig <$> arbitrary
