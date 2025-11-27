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

module Wire.MockInterpreters.TeamCollaboratorsStore where

import Data.Id
import Data.Map qualified as Map
import Data.Maybe
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Team.Collaborator
import Wire.TeamCollaboratorsStore

inMemoryTeamCollaboratorsStoreInterpreter ::
  forall r.
  (Member (State (Map TeamId [TeamCollaborator])) r) =>
  InterpreterFor TeamCollaboratorsStore r
inMemoryTeamCollaboratorsStoreInterpreter =
  interpret $ \case
    CreateTeamCollaborator userId teamId permissions ->
      let teamCollaborator = TeamCollaborator {gUser = userId, gTeam = teamId, gPermissions = permissions}
       in modify $ Map.alter (Just . maybe [teamCollaborator] (teamCollaborator :)) teamId
    GetAllTeamCollaborators teamId ->
      gets $ \(s :: Map TeamId [TeamCollaborator]) -> (fromMaybe []) (Map.lookup teamId s)
    GetTeamCollaborator teamId userId ->
      gets $ \(s :: Map TeamId [TeamCollaborator]) -> find (\tc -> tc.gUser == userId) =<< Map.lookup teamId s
    GetTeamCollaborations userId ->
      gets $ \(s :: Map TeamId [TeamCollaborator]) -> concatMap (filter (\tc -> tc.gUser == userId)) (Map.elems s)
    GetTeamCollaboratorsWithIds teamIds userIds ->
      gets $ \(s :: Map TeamId [TeamCollaborator]) ->
        concatMap (concatMap (filter (\tc -> tc.gUser `elem` userIds)) . (\(tid :: TeamId) -> Map.lookup tid s)) teamIds
    UpdateTeamCollaborator userId teamId permissions ->
      let updatePermissions teamCollaborator =
            if teamCollaborator.gUser == userId
              then teamCollaborator {gPermissions = permissions}
              else teamCollaborator
       in modify $ Map.adjust (fmap updatePermissions) teamId
    RemoveTeamCollaborator userId teamId ->
      modify $ Map.alter (fmap $ filter $ (/= userId) . gUser) teamId
