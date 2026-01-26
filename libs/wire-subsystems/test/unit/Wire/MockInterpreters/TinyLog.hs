-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.TinyLog where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Debug.Trace qualified as Debug
import Imports
import Polysemy
import Polysemy.TinyLog
import System.Logger qualified as Log

noopLogger ::
  Sem (Logger (Log.Msg -> Log.Msg) ': r) a ->
  Sem r a
noopLogger = interpret $ \case
  Log _lvl _msg -> pure ()

debugLogger :: InterpreterFor TinyLog r
debugLogger = interpret $ \case
  Log lvl msg ->
    Debug.traceM $ Text.unpack $ Text.decodeUtf8 $ LBS.toStrict $ Log.render (Log.renderDefault ",") (Log.field "level" (show lvl) . msg)
