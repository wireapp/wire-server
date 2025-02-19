{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module Galley.Cassandra.GetAllTeamFeatures (getAllDbFeaturesLegacy) where

import Cassandra
import Data.Id
import Galley.Cassandra.Instances ()
import Galley.Cassandra.MakeFeature
import Galley.Cassandra.Orphans ()
import Generics.SOP
import Imports hiding (Map)
import Polysemy.Internal
import Wire.API.Team.Feature

type family ConcatFeatureRow xs where
  ConcatFeatureRow '[] = '[]
  ConcatFeatureRow (x : xs) = Append (FeatureRow x) (ConcatFeatureRow xs)

type AllFeatureRow = ConcatFeatureRow Features

emptyRow :: NP Maybe AllFeatureRow
emptyRow = hpure Nothing

class ConcatFeatures cfgs where
  rowToAllFeatures :: NP Maybe (ConcatFeatureRow cfgs) -> NP DbFeature cfgs

instance ConcatFeatures '[] where
  rowToAllFeatures Nil = Nil

instance
  ( SplitNP (FeatureRow cfg) (ConcatFeatureRow cfgs),
    ConcatFeatures cfgs,
    MakeFeature cfg
  ) =>
  ConcatFeatures (cfg : cfgs)
  where
  rowToAllFeatures row = case splitNP @(FeatureRow cfg) @(ConcatFeatureRow cfgs) row of
    (row0, row1) -> rowToFeature row0 :* rowToAllFeatures row1

class SplitNP xs ys where
  splitNP :: NP f (Append xs ys) -> (NP f xs, NP f ys)

instance SplitNP '[] ys where
  splitNP ys = (Nil, ys)

instance (SplitNP xs ys) => SplitNP (x ': xs) ys where
  splitNP (z :* zs) = case splitNP zs of
    (xs, ys) -> (z :* xs, ys)

class AppendNP xs ys where
  appendNP :: NP f xs -> NP f ys -> NP f (Append xs ys)

instance AppendNP '[] ys where
  appendNP Nil ys = ys

instance (AppendNP xs ys) => AppendNP (x : xs) ys where
  appendNP (x :* xs) ys = x :* appendNP xs ys

class ConcatColumns cfgs where
  concatColumns :: NP (K String) (ConcatFeatureRow cfgs)

instance ConcatColumns '[] where
  concatColumns = Nil

instance
  ( AppendNP (FeatureRow cfg) (ConcatFeatureRow cfgs),
    MakeFeature cfg,
    ConcatColumns cfgs
  ) =>
  ConcatColumns (cfg : cfgs)
  where
  concatColumns = featureColumns @cfg `appendNP` concatColumns @cfgs

getAllDbFeaturesLegacy ::
  forall row mrow m.
  ( row ~ AllFeatureRow,
    Tuple (TupleP mrow),
    IsProductType (TupleP mrow) mrow,
    AllZip (IsF Maybe) row mrow,
    MonadClient m
  ) =>
  TeamId ->
  m (AllFeatures DbFeature)
getAllDbFeaturesLegacy tid = do
  mRow <- fetchFeatureRow @row @mrow tid (concatColumns @Features)
  pure . rowToAllFeatures $ fromMaybe emptyRow mRow
