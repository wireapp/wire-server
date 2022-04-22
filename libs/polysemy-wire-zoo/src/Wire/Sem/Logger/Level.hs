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

module Wire.Sem.Logger.Level where

import qualified SAML2.WebSSO as SAML
import qualified System.Logger as Log

-- | The logging level
data Level
  = Fatal
  | Error
  | Warn
  | Info
  | Debug
  | Trace

toLevel :: Level -> Log.Level
toLevel = \case
  Fatal -> Log.Fatal
  Error -> Log.Error
  Warn -> Log.Warn
  Info -> Log.Info
  Debug -> Log.Debug
  Trace -> Log.Trace

fromLevel :: Log.Level -> Level
fromLevel = \case
  Log.Fatal -> Fatal
  Log.Error -> Error
  Log.Warn -> Warn
  Log.Info -> Info
  Log.Debug -> Debug
  Log.Trace -> Trace

samlToLevel :: SAML.Level -> Log.Level
samlToLevel = \case
  SAML.Fatal -> Log.Fatal
  SAML.Error -> Log.Error
  SAML.Warn -> Log.Warn
  SAML.Info -> Log.Info
  SAML.Debug -> Log.Debug
  SAML.Trace -> Log.Trace

samlFromLevel :: SAML.Level -> Level
samlFromLevel = \case
  SAML.Fatal -> Fatal
  SAML.Error -> Error
  SAML.Warn -> Warn
  SAML.Info -> Info
  SAML.Debug -> Debug
  SAML.Trace -> Trace
