-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Application interpreters shared by Gundeck's HTTP handlers. Pools and
-- configuration are created at startup; the request environment supplies the
-- existing Gundeck actions and request context at the HTTP boundary.
module Gundeck.CanonicalInterpreter
  ( GundeckApp,
    runCanonicalInterpreter,
  )
where

import Control.Exception (throwIO)
import Gundeck.Env (Env (..))
import Gundeck.Monad (Gundeck, runDirect)
import Gundeck.NotificationFanIn.Error
import Gundeck.Options (Opts (..), Settings (..))
import Hasql.Pool (UsageError)
import Imports
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Polysemy.Embed (runEmbedded)
import Polysemy.Error
import Wire.FanInNotificationStore
import Wire.FanInNotificationStore.Postgres
import Wire.FanInNotificationSubsystem
import Wire.FanInNotificationSubsystem.Interpreter

type GundeckApp = Sem GundeckEffects

type GundeckEffects =
  '[ FanInNotificationSubsystem,
     FanInNotificationStore,
     Error FanInNotificationSubsystemError,
     Error FanInNotificationStoreError,
     Error UsageError,
     Error Wai.Error,
     Embed Gundeck,
     Embed IO
   ]

runCanonicalInterpreter :: Env -> GundeckApp a -> IO a
runCanonicalInterpreter env action = do
  result <-
    runM
      . runEmbedded (runDirect env)
      . runError @Wai.Error
      . mapError fanInPostgresErrorToHttpError
      . mapError fanInStoreErrorToHttpError
      . mapError fanInSubsystemErrorToHttpError
      . interpretFanInNotificationStoreToPostgres env._notificationFanInPostgresPool
      . interpretFanInNotificationSubsystem env._options._settings._federationDomain
      $ action
  either throwIO pure result
