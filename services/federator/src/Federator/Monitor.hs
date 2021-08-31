-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Federator.Monitor
  ( withMonitor,
    mkTLSSettingsOrThrow,
    FederationSetupError (..),
  )
where

import Control.Exception (bracket, throw)
import Control.Lens (view)
import Federator.Env (Env, TLSSettings (..), applog)
import Federator.Monitor.Internal
import Federator.Options (RunSettings (..))
import Imports
import qualified Polysemy
import qualified Polysemy.Error as Polysemy

mkTLSSettingsOrThrow :: RunSettings -> IO TLSSettings
mkTLSSettingsOrThrow =
  Polysemy.runM
    . (either (Polysemy.embed @IO . throw) pure =<<)
    . Polysemy.runError @FederationSetupError
    . mkTLSSettings

withMonitor :: Env -> RunSettings -> IO a -> IO a
withMonitor env rs action =
  bracket
    (runMonitor (view applog env) (monitorCertificates env rs))
    (runMonitor (view applog env) . stopMonitoringCertificates)
    (const action)
