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

module Federator.Monitor
  ( withMonitor,
    mkTLSSettingsOrThrow,
    FederationSetupError (..),
  )
where

import Control.Exception (bracket, throw)
import Federator.Monitor.Internal
import Federator.Options (RunSettings (..))
import Imports
import OpenSSL.Session (SSLContext)
import qualified Polysemy
import qualified Polysemy.Error as Polysemy
import System.Logger (Logger)

mkTLSSettingsOrThrow :: RunSettings -> IO SSLContext
mkTLSSettingsOrThrow = Polysemy.runM . runEither . Polysemy.runError @FederationSetupError . mkSSLContext
  where
    runEither = (either (Polysemy.embed @IO . throw) pure =<<)

withMonitor :: Logger -> IORef SSLContext -> RunSettings -> IO a -> IO a
withMonitor logger tlsVar rs action =
  bracket
    ( runSemDefault
        logger
        ( mkMonitor
            (runSemDefault logger . logAndIgnoreErrors)
            tlsVar
            rs
        )
    )
    (runSemDefault logger . delMonitor)
    (const action)
