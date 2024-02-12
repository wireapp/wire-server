{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cassandra.Options where

import Cassandra.Helpers
import Control.Lens
import Data.Aeson.TH
import Imports

data Endpoint = Endpoint
  { _host :: !Text,
    _port :: !Word16
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Endpoint

makeLenses ''Endpoint

data CassandraOpts = CassandraOpts
  { _endpoint :: !Endpoint,
    _keyspace :: !Text,
    -- | If this option is unset, use all available nodes.
    -- If this option is set, use only cassandra nodes in the given datacentre
    --
    -- This option is most likely only necessary during a cassandra DC migration
    -- FUTUREWORK: remove this option again, or support a datacentre migration feature
    _filterNodesByDatacentre :: !(Maybe Text),
    _tlsCa :: Maybe FilePath
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''CassandraOpts

makeLenses ''CassandraOpts
