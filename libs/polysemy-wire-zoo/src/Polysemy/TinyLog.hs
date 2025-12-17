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

-- NOTE: This is an obsolete module. Instead, please use the more general
-- Wire.Sem.Logger logging effect.
module Polysemy.TinyLog
  ( module Polysemy.TinyLog,
    Logger (..),
    trace,
    debug,
    info,
    warn,
    err,
    fatal,
  )
where

import Imports
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error
import qualified System.Logger as Log
import Wire.Sem.Logger
import qualified Wire.Sem.Logger as Logger

type TinyLog = Logger (Log.Msg -> Log.Msg)

logErrors ::
  ( Member TinyLog r,
    Member (Error e) r
  ) =>
  (e -> Text) ->
  Text ->
  Sem r a ->
  Sem r a
logErrors showError msg action = Polysemy.Error.catch action $ \e -> do
  Logger.err $ Log.msg msg . Log.field "error" (showError e)
  Polysemy.Error.throw e

logAndIgnoreErrors ::
  forall e r.
  ( Member TinyLog r
  ) =>
  (e -> Text) ->
  Text ->
  Sem (Error e ': r) () ->
  Sem r ()
logAndIgnoreErrors showError msg = void . Polysemy.Error.runError . logErrors showError msg
