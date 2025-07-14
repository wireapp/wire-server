module Wire.TeamCollaboratorsSubsystem.InterpreterSpec where

import Data.Default
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Qualified
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Team.Collaborator
import Wire.API.Team.Member
import Wire.API.Team.Role
import Wire.MiniBackend
import Wire.StoredUser
import Wire.TeamCollaboratorsSubsystem

spec :: Spec
spec = do
  describe "AddTeamCollaborator" $ do
    prop "needs AddTeamCollaborator permission (admin and owner)" $
      \(collaborator :: StoredUser) (owner :: StoredUser) (tid :: TeamId) config ownDomain -> do
        let localBackend :: MiniBackend = def {users = [collaborator, owner]}
            authUser = toLocalUnsafe ownDomain owner.id
            perms = rolePermissions RoleAdmin -- TODO: Test RoleOwner as well
            ownerTeamMember :: TeamMember = mkTeamMember owner.id perms Nothing UserLegalHoldDisabled
            teamMap = Map.singleton tid [ownerTeamMember]
         in runNoFederationStack localBackend teamMap config $
              do
                createTeamCollaborator authUser collaborator.id tid mempty
                collaborators <- getAllTeamCollaborators authUser tid
                pure $ collaborators === [GetTeamCollaborator collaborator.id tid mempty]
