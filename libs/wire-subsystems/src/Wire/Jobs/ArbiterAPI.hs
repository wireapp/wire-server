{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

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
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.Jobs.ArbiterAPI
  ( adminApplication,
  )
where

import Arbiter.Core qualified as ArbiterCore
import Arbiter.Servant.Server qualified as ArbServer
import Arbiter.Servant.UI qualified as ArbUI
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Network.Wai (Application)
import Wire.API.Jobs (ScheduledJobsRegistry)
import Prelude (IO, pure, ($))

-- | Build the Arbiter admin API application for the shared scheduled jobs registry.
adminApplication :: ByteString -> IO Application
adminApplication connStr = do
  config <- ArbServer.initArbiterServer (Proxy @ScheduledJobsRegistry) connStr ArbiterCore.defaultSchemaName
  pure $ ArbUI.arbiterAppWithAdmin config
