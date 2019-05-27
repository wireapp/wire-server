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
import Control.Lens
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Typeable (typeOf)
import Galley.Types.Teams
import Brig.Types.Test.Arbitrary ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


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
    , run @LegalHoldTeamConfig Proxy
    , run @NewLegalHoldService Proxy
    , run @LegalHoldService Proxy
    , run @ViewLegalHoldService Proxy
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
