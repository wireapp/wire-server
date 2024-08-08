{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

module Galley.Cassandra.GetAllTeamFeatureConfigs where

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
  ( Split (FeatureRow cfg) (ConcatFeatureRow cfgs),
    ConcatFeatures cfgs,
    MakeFeature cfg
  ) =>
  ConcatFeatures (cfg : cfgs)
  where
  rowToAllFeatures row = case split @(FeatureRow cfg) @(ConcatFeatureRow cfgs) row of
    (row0, row1) -> rowToFeature row0 :* rowToAllFeatures row1

class Split xs ys where
  split :: NP f (Append xs ys) -> (NP f xs, NP f ys)

instance Split '[] ys where
  split ys = (Nil, ys)

instance (Split xs ys) => Split (x ': xs) ys where
  split (z :* zs) = case split zs of
    (xs, ys) -> (z :* xs, ys)

getAllFeatureConfigs ::
  forall row mrow m.
  ( MonadClient m,
    row ~ AllFeatureRow,
    Tuple (TupleP mrow),
    IsProductType (TupleP mrow) mrow,
    AllZip (IsF Maybe) row mrow
  ) =>
  TeamId ->
  m (AllFeatures DbFeature)
getAllFeatureConfigs tid = do
  mRow <- retry x1 $ query1 select (params LocalQuorum (Identity tid))
  pure $ rowToAllFeatures $ maybe emptyRow (unfactorI . productTypeFrom) mRow
  where
    select :: PrepQuery R (Identity TeamId) (TupleP mrow)
    select = fromString ""
