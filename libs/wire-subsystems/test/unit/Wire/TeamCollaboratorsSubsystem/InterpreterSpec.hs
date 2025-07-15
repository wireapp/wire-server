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
    prop "can create and get team collaborators if the caller has sufficient permissions (admin and owner)" $
      \(collaborator :: StoredUser)
       (owner :: StoredUser)
       (tid :: TeamId)
       config
       ownDomain
       ((EligibleRole role) :: EligibleRole) -> do
          let localBackend :: MiniBackend = def {users = [collaborator, owner]}
              authUser = toLocalUnsafe ownDomain owner.id
              perms = rolePermissions role
              ownerTeamMember :: TeamMember = mkTeamMember owner.id perms Nothing UserLegalHoldDisabled
              teamMap = Map.singleton tid [ownerTeamMember]
           in runNoFederationStack localBackend teamMap config $
                do
                  createTeamCollaborator authUser collaborator.id tid mempty
                  collaborators <- getAllTeamCollaborators authUser tid
                  pure $ collaborators === [GetTeamCollaborator collaborator.id tid mempty]

newtype EligibleRole = EligibleRole Role
  deriving (Eq, Show)

instance Arbitrary EligibleRole where
  arbitrary = EligibleRole <$> elements [RoleAdmin, RoleOwner]
