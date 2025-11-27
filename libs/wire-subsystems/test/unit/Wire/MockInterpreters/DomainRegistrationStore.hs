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

module Wire.MockInterpreters.DomainRegistrationStore where

import Imports
import Polysemy
import Polysemy.State
import Wire.DomainRegistrationStore

inMemoryDomainRegistrationStoreInterpreter :: (Member (State [StoredDomainRegistration]) r) => InterpreterFor DomainRegistrationStore r
inMemoryDomainRegistrationStoreInterpreter = interpret $ \case
  UpsertInternal dr -> modify ((dr :) . filter ((/= domain dr) . domain))
  LookupInternal d -> gets (find ((== d) . domain))
  DeleteInternal d -> modify (filter ((/= d) . domain))
  LookupByTeamInternal tid -> gets (filter ((== Just tid) . authorizedTeam))
