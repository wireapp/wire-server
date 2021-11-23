{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -Wno-orphans             #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.DefaultSsoCodeSpec where

import Arbitrary ()
import Imports
import Polysemy
import Polysemy.Check
import SAML2.WebSSO.Types
import qualified Spar.Sem.DefaultSsoCode as E
import Spar.Sem.DefaultSsoCode.Mem
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

deriveGenericK ''E.DefaultSsoCode

propsForInterpreter ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Spec
propsForInterpreter lower = do
  describe "DefaultSsoCode Actions" $ do
    prop "delete/delete" $ prop_deleteDelete lower
    prop "delete/get"    $ prop_deleteGet lower
    prop "delete/store"  $ prop_deleteStore lower
    prop "get/store"     $ prop_getStore lower
    prop "store/delete"  $ prop_storeStore lower
    prop "store/get"     $ prop_storeGet lower
    prop "store/store"   $ prop_storeStore lower


spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter testInterpreter

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class (Member E.DefaultSsoCode r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z))
   => PropConstraints r f
instance (Member E.DefaultSsoCode r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z))
   => PropConstraints r f


prop_storeGet ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeGet =
  prepropLaw @'[E.DefaultSsoCode] $ do
        s <- arbitrary
        pure
          ( do
              E.store s
              E.get,
            do
              E.store s
              pure (Just s)
          )

prop_getStore ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_getStore =
  prepropLaw @'[E.DefaultSsoCode] $ do
        pure
          ( do
              E.get >>= maybe (pure ()) E.store,
            do
              pure ()
          )

prop_storeDelete ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeDelete =
  prepropLaw @'[E.DefaultSsoCode] $ do
        s <- arbitrary
        pure
          ( do
              E.store s
              E.delete,
            do
              E.delete
          )


prop_deleteStore ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteStore =
  prepropLaw @'[E.DefaultSsoCode] $ do
        s <- arbitrary
        pure
          ( do
              E.delete
              E.store s,
            do
              E.store s
          )

prop_storeStore ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeStore =
  prepropLaw @'[E.DefaultSsoCode] $ do
        s <- arbitrary
        s' <- arbitrary
        pure
          ( do
              E.store s
              E.store s',
            do
              E.store s'
          )

prop_deleteDelete ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteDelete =
  prepropLaw @'[E.DefaultSsoCode] $ do
        pure
          ( do
              E.delete
              E.delete,
            do
              E.delete
          )

prop_deleteGet ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteGet =
  prepropLaw @'[E.DefaultSsoCode] $ do
        pure
          ( do
              E.delete
              E.get,
            do
              E.delete
              pure Nothing
          )

testInterpreter :: Sem '[E.DefaultSsoCode] a -> IO (Maybe IdPId, a)
testInterpreter = pure . run . defaultSsoCodeToMem

