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

module Brig.PolyLog where

import Imports
import Polysemy
import qualified System.Logger as Log

-- | This effect will help us write tests for log messages
--
-- FUTUREWORK: Move this to a separate module if it is required
--
-- FUTUREWORK: Either write an orphan instance for MonadLogger or provide
-- equivalent functions in System.Logger.Class
data PolyLog m a where
  PolyLog :: Log.Level -> (Log.Msg -> Log.Msg) -> PolyLog m ()

makeSem 'PolyLog

runPolyLog :: Member (Embed IO) r => Log.Logger -> Sem (PolyLog ': r) a -> Sem r a
runPolyLog logger = interpret $ \(PolyLog lvl msg) -> Log.log logger lvl msg
