-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Spar.Sem.Logger.TinyLog (loggerToTinyLog, stringLoggerToTinyLog, toLevel, fromLevel) where

import Imports
import Polysemy
import Spar.Sem.Logger (Level (..), Logger (..), mapLogger)
import qualified System.Logger as Log

loggerToTinyLog ::
  Member (Embed IO) r =>
  Log.Logger ->
  Sem (Logger (Log.Msg -> Log.Msg) ': r) a ->
  Sem r a
loggerToTinyLog tinylog = interpret $ \case
  Log lvl msg ->
    embed @IO $ Log.log tinylog (toLevel lvl) msg

stringLoggerToTinyLog :: Member (Logger (Log.Msg -> Log.Msg)) r => Sem (Logger String ': r) a -> Sem r a
stringLoggerToTinyLog = mapLogger @String Log.msg

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
