{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.IdPRawMetadataStoreSpec where

import Arbitrary ()
import Imports
import Polysemy
import Polysemy.Check
import qualified Spar.Sem.IdPRawMetadataStore as E
import Spar.Sem.IdPRawMetadataStore.Mem
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

deriveGenericK ''E.IdPRawMetadataStore

prop_storeGetRaw ::
  Member E.IdPRawMetadataStore r =>
  (forall a. Sem r a -> IO (RawState, a)) ->
  Property
prop_storeGetRaw x =
  prepropLaw @'[E.IdPRawMetadataStore]
    ( do
        idpid <- arbitrary
        t <- arbitrary
        pure
          ( do
              E.store idpid t
              E.get idpid,
            do
              E.store idpid t
              pure (Just t)
          )
    )
    x

prop_storeStoreRaw ::
  Member E.IdPRawMetadataStore r =>
  (forall a. Sem r a -> IO (RawState, a)) ->
  Property
prop_storeStoreRaw x =
  prepropLaw @'[E.IdPRawMetadataStore]
    ( do
        idpid <- arbitrary
        t1 <- arbitrary
        t2 <- arbitrary
        pure
          ( do
              E.store idpid t1
              E.store idpid t2,
            do
              E.store idpid t2
          )
    )
    x

prop_storeDeleteRaw ::
  Member E.IdPRawMetadataStore r =>
  (forall a. Sem r a -> IO (RawState, a)) ->
  Property
prop_storeDeleteRaw x =
  prepropLaw @'[E.IdPRawMetadataStore]
    ( do
        idpid <- arbitrary
        t <- arbitrary
        pure
          ( do
              E.store idpid t
              E.delete idpid,
            do
              E.delete idpid
          )
    )
    x

prop_deleteGetRaw ::
  Member E.IdPRawMetadataStore r =>
  (forall a. Sem r a -> IO (RawState, a)) ->
  Property
prop_deleteGetRaw x =
  prepropLaw @'[E.IdPRawMetadataStore]
    ( do
        idpid <- arbitrary
        pure
          ( do
              E.delete idpid
              E.get idpid,
            do
              E.delete idpid
              pure Nothing
          )
    )
    x

testInterpreter :: Sem '[E.IdPRawMetadataStore] a -> IO (RawState, a)
testInterpreter = pure . run . idpRawMetadataStoreToMem

propsForInterpreter ::
  Member E.IdPRawMetadataStore r =>
  (forall a. Sem r a -> IO (RawState, a)) ->
  Spec
propsForInterpreter lower = do
  prop "store/store" $ prop_storeStoreRaw lower
  prop "store/get" $ prop_storeGetRaw lower
  prop "store/deleteRawMetadata" $ prop_storeDeleteRaw lower
  prop "deleteRawMetadata/get" $ prop_deleteGetRaw lower

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter testInterpreter
