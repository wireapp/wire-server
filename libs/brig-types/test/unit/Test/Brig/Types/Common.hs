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
import Data.Typeable (typeOf)
import Galley.Types
import Galley.Types.Teams
import Galley.Types.Teams.SSO
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


-- NB: validateEveryToJSON from servant-swagger doesn't render these tests unnecessary!

tests :: TestTree
tests = testGroup "Common (types vs. aeson)"
    [ run @Handle
    , run @Name
    , run @ColourId
    , run @Email
    , run @Phone
    , run @UserIdentity
    , run @UserSSOId
    , run @AssetSize
    , run @Asset
    , run @ExcludedPrefix
    , run @ManagedBy
    , run @TeamMemberDeleteData
    , run @LegalHoldStatus
    , run @LegalHoldTeamConfig
    , run @NewLegalHoldService
    , run @LegalHoldService
    , run @ViewLegalHoldService
    , run @NewLegalHoldClient
    , run @RequestNewLegalHoldClient
    , run @UserLegalHoldStatusResponse
    , run @LegalHoldServiceConfirm
    , run @LegalHoldClientRequest
    , run @RemoveLegalHoldSettingsRequest
    , run @DisableLegalHoldForUserRequest
    , run @ApproveLegalHoldForUserRequest
    , run @SSOStatus
    , run @SSOTeamConfig
    , run @FeatureFlags
    , run @ConfigJson
    , configJsonSamples
    , testCase "{} is a valid TeamMemberDeleteData" $ do
        assertEqual "{}" (Right $ newTeamMemberDeleteData Nothing) (eitherDecode "{}")
    ]
  where
    run :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a)
         => TestTree
    run = testProperty msg trip
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

instance Arbitrary FeatureFlags where
  arbitrary = FeatureFlags
      <$> Test.Tasty.QuickCheck.elements [minBound..]
      <*> Test.Tasty.QuickCheck.elements [minBound..]

configJsonSamples :: TestTree
configJsonSamples = testGroup "ConfigJson samples" $ testSample <$>
    [ "{\
      \  \"backend-URI\": \"https://localhost:8080/\",\
      \  \"backend-websocketURI\": \"https://localhost:8080/\",\
      \  \"sso-code\": \"sso_d77d40dc-3788-11ea-8727-0787655d59e1\",\
      \  \"only-sso\": false,\
      \  \"no-account-creation\": false\
      \}"
    , "{\
      \  \"backend-URI\": \"https://localhost:8080/\",\
      \  \"backend-websocketURI\": \"https://localhost:8080/\",\
      \  \"no-account-creation\": false\
      \}"
    ]
  where
    testSample = testCase "sample"
               . assertBool "sample"
               . isRight
               . (eitherDecode @ConfigJson)
