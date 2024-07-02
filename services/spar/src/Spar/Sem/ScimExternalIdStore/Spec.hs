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

module Spar.Sem.ScimExternalIdStore.Spec (propsForInterpreter) where

import Data.Id
import Imports
import Polysemy
import Polysemy.Check
import Spar.Scim.Types (ScimUserCreationStatus)
import qualified Spar.Sem.ScimExternalIdStore as E
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

propsForInterpreter ::
  (PropConstraints r f) =>
  String ->
  (forall a. f a -> a) ->
  (forall a. Sem r a -> IO (f a)) ->
  Spec
propsForInterpreter interpreter extract lower = do
  describe interpreter $ do
    prop "delete/delete" $ prop_deleteDelete Nothing lower
    prop "delete/lookup" $ prop_deleteLookup (Just $ show . void . extract) lower
    prop "delete/insert" $ prop_deleteInsert Nothing lower
    prop "lookup/insert" $ prop_lookupInsert Nothing lower
    prop "insert/delete" $ prop_insertDelete Nothing lower
    prop "insert/lookup" $ prop_insertLookup (Just $ show . void . extract) lower
    prop "insert/insert" $ prop_insertInsert (Just $ show . void . extract) lower

-- FUTUREWORK: Add prop tests for missing operations

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class
  (Arbitrary UserId, CoArbitrary UserId, Arbitrary ScimUserCreationStatus, CoArbitrary ScimUserCreationStatus, Functor f, Member E.ScimExternalIdStore r, forall z. (Show z) => Show (f z), forall z. (Eq z) => Eq (f z)) =>
  PropConstraints r f

instance
  (CoArbitrary UserId, CoArbitrary ScimUserCreationStatus, Functor f, Member E.ScimExternalIdStore r, forall z. (Show z) => Show (f z), forall z. (Eq z) => Eq (f z)) =>
  PropConstraints r f

prop_insertLookup ::
  (PropConstraints r f) =>
  Maybe (f (Maybe UserId) -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_insertLookup =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    uid <- arbitrary
    pure $
      simpleLaw
        ( do
            E.insert tid email uid
            E.lookup tid email
        )
        ( do
            E.insert tid email uid
            pure (Just uid)
        )

prop_lookupInsert ::
  (PropConstraints r f) =>
  Maybe (f () -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_lookupInsert =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    pure $
      simpleLaw
        ( do
            E.lookup tid email >>= maybe (pure ()) (E.insert tid email)
        )
        ( do
            pure ()
        )

prop_insertDelete ::
  (PropConstraints r f) =>
  Maybe (f () -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_insertDelete =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    uid <- arbitrary
    pure $
      simpleLaw
        ( do
            E.insert tid email uid
            E.delete tid email
        )
        ( do
            E.delete tid email
        )

prop_deleteInsert ::
  (PropConstraints r f) =>
  Maybe (f () -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteInsert =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    uid <- arbitrary
    pure $
      simpleLaw
        ( do
            E.delete tid email
            E.insert tid email uid
        )
        ( do
            E.insert tid email uid
        )

prop_insertInsert ::
  (PropConstraints r f) =>
  Maybe (f (Maybe UserId) -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_insertInsert =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    uid <- arbitrary
    uid' <- arbitrary
    pure $
      simpleLaw
        ( do
            E.insert tid email uid
            E.insert tid email uid'
            E.lookup tid email
        )
        ( do
            E.insert tid email uid'
            E.lookup tid email
        )

prop_deleteDelete ::
  (PropConstraints r f) =>
  Maybe (f () -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteDelete =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    pure $
      simpleLaw
        ( do
            E.delete tid email
            E.delete tid email
        )
        ( do
            E.delete tid email
        )

prop_deleteLookup ::
  (PropConstraints r f) =>
  Maybe (f (Maybe UserId) -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteLookup =
  prepropLaw @'[E.ScimExternalIdStore] $ do
    tid <- arbitrary
    email <- arbitrary
    uid <- arbitrary
    pure $
      Law
        { lawLhs = do
            E.delete tid email
            E.lookup tid email,
          lawRhs = do
            E.delete tid email
            pure Nothing,
          lawPrelude = [E.insert tid email uid],
          lawPostlude = [] @(Sem _ ())
        }
