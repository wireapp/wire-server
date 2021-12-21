{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.DefaultSsoCodeSpec where

import Arbitrary ()
import Imports
import Polysemy
import Spar.Sem.DefaultSsoCode.Mem
import Spar.Sem.DefaultSsoCode.Spec
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter "defaultSsoCodeToMem" $ pure . run . defaultSsoCodeToMem
