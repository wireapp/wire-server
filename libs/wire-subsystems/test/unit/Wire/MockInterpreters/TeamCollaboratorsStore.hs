module Wire.MockInterpreters.TeamCollaboratorsStore where

import Data.Id
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Team.Collaborator
import Wire.TeamCollaboratorsStore

inMemoryTeamCollaboratorsStoreInterpreter ::
  forall r.
  (Member (State (Map TeamId [GetTeamCollaborator])) r) =>
  InterpreterFor TeamCollaboratorsStore r
inMemoryTeamCollaboratorsStoreInterpreter =
  interpret $ \case
    CreateTeamCollaborator userId teamId permissions ->
      let teamCollaborator = GetTeamCollaborator userId teamId (Set.toList permissions)
       in modify $ Map.alter (Just . maybe [teamCollaborator] (teamCollaborator :)) teamId
    GetAllTeamCollaborators teamId ->
      gets $ \(s :: Map TeamId [GetTeamCollaborator]) -> (maybe [] id) (Map.lookup teamId s)
