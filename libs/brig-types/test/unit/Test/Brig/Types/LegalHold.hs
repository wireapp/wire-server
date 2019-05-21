{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Brig.Types.LegalHold where

import Imports
import Brig.Types.Team.LegalHold
import Data.Aeson
import Data.Aeson.Types
import Data.Typeable (typeOf)
import Test.Brig.Types.Arbitrary ()
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "LegalHold Roundtrips" roundtripTests

roundtripTests :: [TestTree]
roundtripTests =
    [ run @LegalHoldEnabled
    , run @NewLegalHoldService
    , run @LegalHoldService
    , run @ViewLegalHoldService
    ]
  where
    run :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a)
        => TestTree
    run = testProperty msg trip
      where
        msg = show $ typeOf (undefined :: a)
        trip (v :: a) = counterexample (show $ toJSON v)
                      $ Right v === (parseEither parseJSON . toJSON) v



