{-# LANGUAGE QuantifiedConstraints       #-}
{-# OPTIONS_GHC -Wno-orphans             #-}
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

class
  (Functor f, Member E.IdPRawMetadataStore r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z)) =>
  PropConstraints r f

instance
  (Functor f, Member E.IdPRawMetadataStore r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z)) =>
  PropConstraints r f

prop_storeGetRaw ::
  PropConstraints r f =>
  Maybe (f (Maybe Text) -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeGetRaw =
  prepropLaw @'[E.IdPRawMetadataStore]
    ( do
        idpid <- arbitrary
        t <- arbitrary
        pure $ simpleLaw
          ( do
              E.store idpid t
              E.get idpid)
          ( do
              E.store idpid t
              pure (Just t)
          )
    )

prop_storeStoreRaw ::
  PropConstraints r f =>
  Maybe (f (Maybe Text) -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeStoreRaw =
  prepropLaw @'[E.IdPRawMetadataStore]
    ( do
        idpid <- arbitrary
        t1 <- arbitrary
        t2 <- arbitrary
        pure $ simpleLaw
          ( do
              E.store idpid t1
              E.store idpid t2
              E.get idpid
          )
            (do
              E.store idpid t2
              E.get idpid
          )
    )

prop_storeDeleteRaw ::
  PropConstraints r f =>
  Maybe (f (Maybe Text) -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeDeleteRaw =
  prepropLaw @'[E.IdPRawMetadataStore] $
    do
        idpid <- arbitrary
        t <- arbitrary
        pure $ simpleLaw
          ( do
              E.store idpid t
              E.delete idpid
              E.get idpid
          )
          ( do
              E.delete idpid
              E.get idpid
          )

prop_deleteGetRaw ::
  PropConstraints r f =>
  Maybe (f (Maybe Text) -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteGetRaw =
  prepropLaw @'[E.IdPRawMetadataStore]
    ( do
        idpid <- arbitrary
        t <- arbitrary
        pure $ Law
          { lawLhs = do
              E.delete idpid
              E.get idpid
          , lawRhs = do
              E.delete idpid
              pure Nothing
          , lawPrelude =
              [ E.store idpid t
              ]
          , lawPostlude = [] @(Sem _ ())
          }
    )

testInterpreter :: Sem '[E.IdPRawMetadataStore] a -> IO (RawState, a)
testInterpreter = pure . run . idpRawMetadataStoreToMem

propsForInterpreter ::
  PropConstraints r f =>
  (forall x. f x -> x) ->
  (forall a. Sem r a -> IO (f a)) ->
  Spec
propsForInterpreter extract lower = do
  prop "store/store" $ prop_storeStoreRaw (Just $ constructorLabel . extract) lower
  prop "store/get" $ prop_storeGetRaw (Just $ constructorLabel . extract) lower
  prop "store/delete" $ prop_storeDeleteRaw (Just $ constructorLabel . extract) lower
  prop "delete/get" $ prop_deleteGetRaw (Just $ constructorLabel . extract) lower

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter snd testInterpreter

