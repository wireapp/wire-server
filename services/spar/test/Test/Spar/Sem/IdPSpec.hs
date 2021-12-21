{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.IdPSpec where

import Arbitrary ()
import Imports
import Polysemy
import Spar.Sem.IdP.Mem
import Spar.Sem.IdP.Spec
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter "idPToMem" snd (Just $ show . snd) $ pure . run . idPToMem
