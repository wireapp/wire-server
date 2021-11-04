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

module Galley.Data.LegalHold
  ( createSettings,
    getSettings,
    removeSettings,
    Galley.Data.LegalHold.insertPendingPrekeys,
    Galley.Data.LegalHold.selectPendingPrekeys,
    Galley.Data.LegalHold.dropPendingPrekeys,
    setUserLegalHoldStatus,
    setTeamLegalholdWhitelisted,
    isTeamLegalholdWhitelisted,
    unsetTeamLegalholdWhitelisted,
  )
where

import Brig.Types.Client.Prekey
import Brig.Types.Instances ()
import Brig.Types.Team.LegalHold
import Cassandra
import Control.Lens (unsnoc, view)
import Data.Id
import Data.LegalHold
import qualified Galley.Cassandra.LegalHold as C
import Galley.Data.Instances ()
import Galley.Data.Queries as Q
import Galley.Env
import qualified Galley.Options as Opts
import Galley.Types.Teams (flagLegalHold)
import Imports
