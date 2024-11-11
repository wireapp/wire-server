{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cassandra.Options where

import Data.Aeson.TH
import Imports

data Endpoint = Endpoint
  { host :: !Text,
    port :: !Word16
  }
  deriving (Show, Eq, Generic)

deriveFromJSON defaultOptions ''Endpoint

data CassandraOpts = CassandraOpts
  { endpoint :: !Endpoint,
    keyspace :: !Text,
    -- | If this option is unset, use all available nodes.
    -- If this option is set, use only cassandra nodes in the given datacentre
    --
    -- This option is most likely only necessary during a cassandra DC migration
    -- FUTUREWORK: remove this option again, or support a datacentre migration feature
    filterNodesByDatacentre :: !(Maybe Text),
    tlsCa :: Maybe FilePath
  }
  deriving (Show, Eq, Generic)

deriveFromJSON defaultOptions ''CassandraOpts
