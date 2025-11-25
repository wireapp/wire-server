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

module Wire.MockInterpreters.SparAPIAccess where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.User.IdentityProvider
import Wire.SparAPIAccess

-- | interprets galley by statically returning the values passed
miniSparAPIAccess :: (Member (Input (Map TeamId IdPList)) r) => InterpreterFor SparAPIAccess r
miniSparAPIAccess = interpret $ \case
  GetIdentityProviders tid ->
    Map.findWithDefault (IdPList []) tid <$> input
  DeleteTeam {} -> error "DeleteTeam not implemented in miniSparAPIAccess"
  LookupScimUserInfo {} -> error "LookupScimUserInfo not implemented in miniSparAPIAccess"

emptySparAPIAccess :: InterpreterFor SparAPIAccess r
emptySparAPIAccess = runInputConst mempty . miniSparAPIAccess . raiseUnder
