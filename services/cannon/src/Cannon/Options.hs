{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

module Cannon.Options
  ( host,
    port,
    cannon,
    gundeck,
    externalHost,
    externalHostFile,
    logLevel,
    logNetStrings,
    logFormat,
    drainOpts,
    Opts,
    gracePeriodSeconds,
    millisecondsBetweenBatches,
    minBatchSize,
    disabledAPIVersions,
    DrainOpts,
    optsSchema,
  )
where

-- TODO: this is all nice, but how do we process the yaml schemas once we have them?  are they
-- human-readable?  check the ticket again!

-- TODO: move Cannon.Options to wire-subsystems.  :
-- "Wire.Options.Cannon", ...; and "Wire.Options" for the concatenation.  (and find a good reason to put it there.)

import Control.Lens (makeFields)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as A
import Data.Json.Util
import Data.OpenApi qualified as O
import Data.OpenApi.Declare qualified as O
import Data.Proxy (Proxy (Proxy))
import Data.Schema
import Data.Set qualified as Set
import Imports
import System.Logger.Extended (Level, LogFormat)
import Wire.API.Routes.Version

data Cannon = Cannon
  { _cannonHost :: !String,
    _cannonPort :: !Word16,
    _cannonExternalHost :: !(Maybe Text),
    _cannonExternalHostFile :: !(Maybe FilePath)
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, O.ToSchema) via (Schema Cannon)

instance ToSchema Cannon where
  schema =
    object "Cannon.Options.Cannon" $
      Cannon
        <$> _cannonHost .= field "host" schema
        <*> _cannonPort .= field "port" schema
        <*> _cannonExternalHost .= maybe_ (optField "externalHost" schema)
        <*> _cannonExternalHostFile .= maybe_ (optField "externalHostFile" schema)

makeFields ''Cannon

data Gundeck = Gundeck
  { _gundeckHost :: !Text,
    _gundeckPort :: !Word16
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, O.ToSchema) via (Schema Gundeck)

instance ToSchema Gundeck where
  schema =
    object "Cannon.Options.Gundeck" $
      Gundeck
        <$> _gundeckHost .= field "host" schema
        <*> _gundeckPort .= field "port" schema

makeFields ''Gundeck

data DrainOpts = DrainOpts
  { -- | Maximum amount of time draining should take. Must not be set to 0.
    _drainOptsGracePeriodSeconds :: Word64,
    -- | Maximum amount of time between batches, this speeds up draining in case
    -- there are not many users connected. Must not be set to 0.
    _drainOptsMillisecondsBetweenBatches :: Word64,
    -- | Batch size is calculated considering actual number of websockets and
    -- gracePeriod. If this number is too little, '_drainOptsMinBatchSize' is
    -- used.
    _drainOptsMinBatchSize :: Word64
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, O.ToSchema) via (Schema DrainOpts)

instance ToSchema DrainOpts where
  schema =
    object "Cannon.Options.DrainOpts" $
      DrainOpts
        <$> _drainOptsGracePeriodSeconds .= field "gracePeriodSeconds" schema
        <*> _drainOptsMillisecondsBetweenBatches .= field "millisecondsBetweenBatches" schema
        <*> _drainOptsMinBatchSize .= field "minBatchSize" schema

makeFields ''DrainOpts

data Opts = Opts
  { _optsCannon :: !Cannon,
    _optsGundeck :: !Gundeck,
    _optsLogLevel :: !Level,
    _optsLogNetStrings :: !(Maybe (Last Bool)),
    _optsLogFormat :: !(Maybe (Last LogFormat)),
    _optsDrainOpts :: DrainOpts,
    _optsDisabledAPIVersions :: !(Set VersionExp)
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, O.ToSchema) via (Schema Opts)

instance ToSchema Opts where
  schema =
    object "Cannon.Options.Opts" $
      Opts
        <$> _optsCannon .= field "cannon" schema
        <*> _optsGundeck .= field "gundeck" schema
        <*> _optsLogLevel .= field "logLevel" schema
        <*> (fmap getLast . _optsLogNetStrings) .= maybe_ (optField "logNetStrings" (Last <$> schema))
        <*> (fmap getLast . _optsLogFormat) .= maybe_ (optField "logFormat" (Last <$> schema))
        <*> _optsDrainOpts .= field "drainOpts" schema
        <*> (Set.toList . _optsDisabledAPIVersions) .= field "disabledAPIVersions" (Set.fromList <$> array schema)

makeFields ''Opts

optsSchema :: JsonObject
optsSchema = case A.toJSON r of
  A.Object obj -> JsonObject obj
  other -> JsonObject (A.singleton "value" other)
  where
    d :: O.Declare (O.Definitions O.Schema) O.Schema
    d = O.declareSchema (Proxy @Opts)

    r :: (O.Definitions O.Schema, O.Schema)
    r = O.runDeclare d mempty
