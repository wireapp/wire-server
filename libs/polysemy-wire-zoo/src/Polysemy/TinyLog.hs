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
module Polysemy.TinyLog where

import Imports
import Polysemy
import System.Logger (Level (..))
import qualified System.Logger as Log

data TinyLog m a where
  Polylog :: Log.Level -> (Log.Msg -> Log.Msg) -> TinyLog m ()

makeSem ''TinyLog

runTinyLog :: Member (Embed IO) r => Log.Logger -> Sem (TinyLog ': r) a -> Sem r a
runTinyLog logger = interpret $ \(Polylog lvl msg) -> Log.log logger lvl msg

-- | Abbreviation of 'log' using the corresponding log level.
trace, debug, info, warn, err, fatal :: Member TinyLog r => (Log.Msg -> Log.Msg) -> Sem r ()
trace = polylog Trace
debug = polylog Debug
info = polylog Info
warn = polylog Warn
err = polylog Error
fatal = polylog Fatal
{-# INLINE trace #-}
{-# INLINE debug #-}
{-# INLINE info #-}
{-# INLINE warn #-}
{-# INLINE err #-}
{-# INLINE fatal #-}
