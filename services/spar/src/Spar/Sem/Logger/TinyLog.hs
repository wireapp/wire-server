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

module Spar.Sem.Logger.TinyLog
  ( loggerToTinyLog,
    stringLoggerToTinyLog,
    module Spar.Sem.Logger.Level,
  )
where

import Imports
import Polysemy
import Spar.Sem.Logger
import Spar.Sem.Logger.Level
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
