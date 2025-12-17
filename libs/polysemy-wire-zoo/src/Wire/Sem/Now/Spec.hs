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

module Wire.Sem.Now.Spec (propsForInterpreter) where

import Imports
import Polysemy
import Polysemy.Check
import Polysemy.Input
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Wire.Sem.Now as E

propsForInterpreter ::
  (PropConstraints r f) =>
  String ->
  (forall a. Sem r a -> IO (f a)) ->
  Spec
propsForInterpreter interpreter lower = do
  describe interpreter $ do
    prop "now/now" $ prop_nowNow Nothing lower

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class
  (Functor f, Member E.Now r, Member (Input ()) r, forall z. (Show z) => Show (f z), forall z. (Eq z) => Eq (f z)) =>
  PropConstraints r f

instance
  (Functor f, Member E.Now r, Member (Input ()) r, forall z. (Show z) => Show (f z), forall z. (Eq z) => Eq (f z)) =>
  PropConstraints r f

prop_nowNow ::
  (PropConstraints r f) =>
  Maybe (f Bool -> String) ->
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_nowNow =
  -- NOTE: This @Input ()@ effect is a workaround to an oversight in
  -- @polysemy-check@. 'prepropLaw' wants to synthesize some actions to run
  -- before and after its generators, and check their results for equality. We
  -- can't use 'Now' as this effect, because 'E.get' won't return equivalent
  -- results! And we can't keep it empty, because that triggers a crash in
  -- @polysemy-check@. Thus @Input ()@, which isn't beautiful, but works fine.
  prepropLaw @'[Input ()] $ do
    pure $
      simpleLaw
        (liftA2 (<=) E.get E.get)
        ( pure True
        )
