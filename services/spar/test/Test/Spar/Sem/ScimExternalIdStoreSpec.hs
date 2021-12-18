{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.ScimExternalIdStoreSpec where

import Arbitrary ()
import Imports
import Polysemy
import Spar.Sem.ScimExternalIdStore.Mem
import Spar.Sem.ScimExternalIdStore.Spec
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter "scimExternalIdStoreToMem" snd $ pure . run . scimExternalIdStoreToMem

