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
    RemoveTeamCollaborator userId teamId ->
      modify $ Map.alter (fmap $ filter $ (/= userId) . gUser) teamId
