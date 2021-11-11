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

module Galley.Effects.LegalHoldStore
  ( LegalHoldStore (..),
    createSettings,
    getSettings,
    removeSettings,
    insertPendingPrekeys,
    selectPendingPrekeys,
    dropPendingPrekeys,
    setUserLegalHoldStatus,
    setTeamLegalholdWhitelisted,
    unsetTeamLegalholdWhitelisted,
    isTeamLegalholdWhitelisted,
  )
where

import Data.Id
import Data.LegalHold
import Galley.External.LegalHoldService.Types
import Imports
import Polysemy
import Wire.API.User.Client.Prekey

data LegalHoldStore m a where
  CreateSettings :: LegalHoldService -> LegalHoldStore m ()
  GetSettings :: TeamId -> LegalHoldStore m (Maybe LegalHoldService)
  RemoveSettings :: TeamId -> LegalHoldStore m ()
  InsertPendingPrekeys :: UserId -> [Prekey] -> LegalHoldStore m ()
  SelectPendingPrekeys :: UserId -> LegalHoldStore m (Maybe ([Prekey], LastPrekey))
  DropPendingPrekeys :: UserId -> LegalHoldStore m ()
  SetUserLegalHoldStatus :: TeamId -> UserId -> UserLegalHoldStatus -> LegalHoldStore m ()
  SetTeamLegalholdWhitelisted :: TeamId -> LegalHoldStore m ()
  UnsetTeamLegalholdWhitelisted :: TeamId -> LegalHoldStore m ()
  IsTeamLegalholdWhitelisted :: TeamId -> LegalHoldStore m Bool

makeSem ''LegalHoldStore
