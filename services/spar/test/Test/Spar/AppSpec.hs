{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Test.Spar.AppSpec where

import Imports
import Arbitrary ()
import Data.String.Conversions
import Spar.App
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "condenseLogMsg" $ do
    it "behaves like the old, slow implementation" . property $ \(someText :: ST) -> do
      let oldAndSlow :: ST -> ST
          oldAndSlow = cs . dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse . f . fmap flatten . cs
            where
              f (' ' : ' ' : xs) = f (' ' : xs)
              f (x : xs)         = x : f xs
              f []               = []

              flatten '\n' = ' '
              flatten '\r' = ' '
              flatten '\t' = ' '
              flatten '\v' = ' '
              flatten '\f' = ' '
              flatten c    = c

      oldAndSlow someText === condenseLogMsg someText
