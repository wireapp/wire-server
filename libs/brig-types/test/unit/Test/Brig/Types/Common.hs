{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Brig.Types.Common where

import Brig.Types.Common
import Data.Aeson
import Data.Aeson.Types
import Data.Typeable
import Test.Brig.Types.Arbitrary ()
import Test.Tasty
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
    ]
  where
    run :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a)
         => Proxy a -> TestTree
    run Proxy = testProperty msg trip
      where
        msg = show $ typeOf (undefined :: a)
        trip (v :: a) = counterexample (show $ toJSON v)
                      $ Right v === (parseEither parseJSON . toJSON) v
