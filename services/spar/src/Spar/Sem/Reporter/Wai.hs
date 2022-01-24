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

module Spar.Sem.Reporter.Wai where

import Imports
import qualified Network.Wai.Utilities.Server as Wai
import Polysemy
import Polysemy.Input
import Spar.Sem.Reporter
import qualified System.Logger as TinyLog

reporterToTinyLogWai :: Members '[Embed IO, Input TinyLog.Logger] r => Sem (Reporter ': r) a -> Sem r a
reporterToTinyLogWai = interpret $ \case
  Report req err -> do
    logger <- input
    embed @IO $ Wai.logError logger req err
