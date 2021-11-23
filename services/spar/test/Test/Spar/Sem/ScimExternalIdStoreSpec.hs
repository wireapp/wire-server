{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.ScimExternalIdStoreSpec where

import Arbitrary ()
import Imports
import Polysemy
import Polysemy.Check
import qualified Spar.Sem.ScimExternalIdStore as E
import Spar.Sem.ScimExternalIdStore.Mem
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

deriveGenericK ''E.ScimExternalIdStore

propsForInterpreter ::
  PropConstraints r f =>
  String ->
  (forall a. Sem r a -> IO (f a)) ->
  Spec
propsForInterpreter interpreter lower = do
  describe interpreter $ do
    prop "delete/delete" $ prop_deleteDelete lower
    prop "delete/lookup" $ prop_deleteLookup lower
    prop "delete/insert" $ prop_deleteInsert lower
    prop "lookup/insert" $ prop_lookupInsert lower
    prop "insert/delete" $ prop_insertInsert lower
    prop "insert/lookup" $ prop_insertLookup lower
    prop "insert/insert" $ prop_insertInsert lower

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter "scimExternalIdStoreToMem" $ pure . run . scimExternalIdStoreToMem

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class
  (Member E.ScimExternalIdStore r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z)) =>
  PropConstraints r f

instance
  (Member E.ScimExternalIdStore r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z)) =>
  PropConstraints r f

prop_insertLookup ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_insertLookup =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    uid <- arbitrary
    pure
      ( do
          E.insert tid email uid
          E.lookup tid email,
        do
          E.insert tid email uid
          pure (Just uid)
      )

prop_lookupInsert ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_lookupInsert =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    pure
      ( do
          E.lookup tid email >>= maybe (pure ()) (E.insert tid email),
        do
          pure ()
      )

prop_insertDelete ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_insertDelete =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    uid <- arbitrary
    pure
      ( do
          E.insert tid email uid
          E.delete tid email,
        do
          E.delete tid email
      )

prop_deleteInsert ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteInsert =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    uid <- arbitrary
    pure
      ( do
          E.delete tid email
          E.insert tid email uid,
        do
          E.insert tid email uid
      )

prop_insertInsert ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_insertInsert =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    uid <- arbitrary
    uid' <- arbitrary
    pure
      ( do
          E.insert tid email uid
          E.insert tid email uid',
        do
          E.insert tid email uid'
      )

prop_deleteDelete ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteDelete =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    pure
      ( do
          E.delete tid email
          E.delete tid email,
        do
          E.delete tid email
      )

prop_deleteLookup ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteLookup =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    pure
      ( do
          E.delete tid email
          E.lookup tid email,
        do
          E.delete tid email
          pure Nothing
      )
