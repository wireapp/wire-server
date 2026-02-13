{-# LANGUAGE RecordWildCards #-}

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

module Wire.Sem.Logger.TinyLog
  ( loggerToTinyLog,
    loggerToTinyLogReqId,
    stringLoggerToTinyLog,
    discardTinyLogs,
    module System.Logger,
    RecordedLog,
    LogRecorder (..),
    newLogRecorder,
    recordLogs,
    pureRecordLogs,
  )
where

import Data.Id
import Imports
import Polysemy
import Polysemy.State
import Polysemy.TinyLog (TinyLog)
import System.Logger (Level (..))
import qualified System.Logger as Log
import Wire.Sem.Logger

loggerToTinyLog ::
  (Member (Embed IO) r) =>
  Log.Logger ->
  Sem (Logger (Log.Msg -> Log.Msg) ': r) a ->
  Sem r a
loggerToTinyLog tinylog = interpret $ \case
  Log lvl msg ->
    embed @IO $ Log.log tinylog lvl msg

-- | Log the request ID along with the message
loggerToTinyLogReqId ::
  (Member (Embed IO) r) =>
  RequestId ->
  Log.Logger ->
  Sem (TinyLog ': r) a ->
  Sem r a
loggerToTinyLogReqId r tinylog =
  loggerToTinyLog tinylog
    . mapLogger (Log.field "request" (unRequestId r) .)
    . raiseUnder @TinyLog

stringLoggerToTinyLog :: (Member (Logger (Log.Msg -> Log.Msg)) r) => Sem (Logger String ': r) a -> Sem r a
stringLoggerToTinyLog = mapLogger @String Log.msg

discardTinyLogs :: Sem (Logger (Log.Msg -> Log.Msg) ': r) a -> Sem r a
discardTinyLogs = discardLogs

type RecordedLog = (Log.Level, LByteString)

newtype LogRecorder = LogRecorder {recordedLogs :: IORef [RecordedLog]}

newLogRecorder :: IO LogRecorder
newLogRecorder = LogRecorder <$> newIORef []

recordLogs :: (Member (Embed IO) r) => LogRecorder -> Sem (TinyLog ': r) a -> Sem r a
recordLogs LogRecorder {..} = interpret $ \(Log lvl msg) ->
  modifyIORef' recordedLogs (++ [(lvl, renderMsg msg)])

-- | Interpret `TinyLog` pure (without `IO`)
--
-- This function is meant to be used in unit tests, when you want to record
-- logs without adding `IO` interpreters to the interpreter stack.
pureRecordLogs ::
  (Member (State [RecordedLog]) r) =>
  InterpreterFor TinyLog r
pureRecordLogs = interpret $ \case
  Wire.Sem.Logger.Log lvl msg -> modify ((lvl, renderMsg msg) :)

renderMsg :: (Log.Msg -> Log.Msg) -> LByteString
renderMsg = Log.render (Log.renderDefault ", ")
