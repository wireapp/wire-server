{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.IdPRawMetadataStoreSpec where

import Arbitrary ()
import Imports
import Polysemy
import qualified Spar.Sem.IdPRawMetadataStore as E
import Spar.Sem.IdPRawMetadataStore.Mem
import Spar.Sem.IdPRawMetadataStore.Spec
import Test.Hspec
import Test.Hspec.QuickCheck

testInterpreter :: Sem '[E.IdPRawMetadataStore] a -> IO (RawState, a)
testInterpreter = pure . run . idpRawMetadataStoreToMem

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter snd testInterpreter
