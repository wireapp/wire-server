{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Galley.Cassandra.Orphans where

import Galley.Cassandra.FeatureTH

$generateSOPInstances
