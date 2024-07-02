{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Spar.Sem.DefaultSsoCode.Spec (propsForInterpreter) where

import Imports
import Polysemy
import Polysemy.Check
import SAML2.WebSSO.Types
import qualified Spar.Sem.DefaultSsoCode as E
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

propsForInterpreter ::
  (PropConstraints r f) =>
  String ->
  (forall a. Sem r a -> IO (f a)) ->
  Spec
propsForInterpreter interpreter lower = do
  describe interpreter $ do
    prop "delete/delete" $ prop_deleteDelete Nothing lower
    prop "delete/get" $ prop_deleteGet Nothing lower
    prop "delete/store" $ prop_deleteStore Nothing lower
    prop "get/store" $ prop_getStore Nothing lower
    prop "store/delete" $ prop_storeDelete Nothing lower
    prop "store/get" $ prop_storeGet Nothing lower
    prop "store/store" $ prop_storeStore Nothing lower

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class
  (Arbitrary IdPId, CoArbitrary IdPId, Functor f, Member E.DefaultSsoCode r, forall z. (Show z) => Show (f z), forall z. (Eq z) => Eq (f z)) =>
  PropConstraints r f

instance
  (Arbitrary IdPId, CoArbitrary IdPId, Functor f, Member E.DefaultSsoCode r, forall z. (Show z) => Show (f z), forall z. (Eq z) => Eq (f z)) =>
  PropConstraints r f

prop_storeGet ::
  (PropConstraints r f) =>
  Maybe (f (Maybe IdPId) -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeGet =
  prepropLaw @'[E.DefaultSsoCode] $ do
    s <- arbitrary
    pure $
      simpleLaw
        ( do
            E.store s
            E.get
        )
        ( do
            E.store s
            pure (Just s)
        )

prop_getStore ::
  (PropConstraints r f) =>
  Maybe (f () -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_getStore =
  prepropLaw @'[E.DefaultSsoCode] $ do
    pure $
      simpleLaw
        ( do
            E.get >>= maybe (pure ()) E.store
        )
        ( do
            pure ()
        )

prop_storeDelete ::
  (PropConstraints r f) =>
  Maybe (f () -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeDelete =
  prepropLaw @'[E.DefaultSsoCode] $ do
    s <- arbitrary
    pure $
      simpleLaw
        ( do
            E.store s
            E.delete
        )
        ( do
            E.delete
        )

prop_deleteStore ::
  (PropConstraints r f) =>
  Maybe (f () -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteStore =
  prepropLaw @'[E.DefaultSsoCode] $ do
    s <- arbitrary
    pure $
      simpleLaw
        ( do
            E.delete
            E.store s
        )
        ( do
            E.store s
        )

prop_storeStore ::
  (PropConstraints r f) =>
  Maybe (f () -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeStore =
  prepropLaw @'[E.DefaultSsoCode] $ do
    s <- arbitrary
    s' <- arbitrary
    pure $
      simpleLaw
        ( do
            E.store s
            E.store s'
        )
        ( do
            E.store s'
        )

prop_deleteDelete ::
  (PropConstraints r f) =>
  Maybe (f () -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteDelete =
  prepropLaw @'[E.DefaultSsoCode] $ do
    pure $
      simpleLaw
        ( do
            E.delete
            E.delete
        )
        ( do
            E.delete
        )

prop_deleteGet ::
  (PropConstraints r f) =>
  Maybe (f (Maybe IdPId) -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteGet =
  prepropLaw @'[E.DefaultSsoCode] $ do
    pure $
      simpleLaw
        ( do
            E.delete
            E.get
        )
        ( do
            E.delete
            pure Nothing
        )
