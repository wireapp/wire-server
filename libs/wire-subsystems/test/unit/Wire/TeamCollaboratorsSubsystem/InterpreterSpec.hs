module Wire.TeamCollaboratorsSubsystem.InterpreterSpec where

import Data.Default
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Qualified
import Data.Set qualified as Set
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Team.Collaborator
import Wire.API.Team.Member
import Wire.API.Team.Role
import Wire.MiniBackend
import Wire.MockInterpreters.Error
import Wire.StoredUser
import Wire.TeamCollaboratorsSubsystem

spec :: Spec
spec = do
  describe "CreateTeamCollaborator" $ do
    prop "can create and get team collaborators if the caller has sufficient permissions (admin and owner)" $
      \(collaborator :: StoredUser)
       (owner :: StoredUser)
       (tid :: TeamId)
       config
       ownDomain
       collabPerms
       ((EligibleRole role) :: EligibleRole) -> do
          let localBackend :: MiniBackend = def {users = [collaborator, owner]}
              authUser = toLocalUnsafe ownDomain owner.id
              perms = rolePermissions role
              ownerTeamMember :: TeamMember = mkTeamMember owner.id perms Nothing UserLegalHoldDisabled
              teamMap = Map.singleton tid [ownerTeamMember]
           in runNoFederationStack localBackend teamMap config $
                do
                  createTeamCollaborator authUser collaborator.id tid collabPerms
                  collaborators <- getAllTeamCollaborators authUser tid
                  pure $ collaborators === [TeamCollaborator collaborator.id tid collabPerms]

    prop "can get empty team collaborator list" $
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
                (mempty ===) <$> getAllTeamCollaborators authUser tid

    prop "creation fails if the caller has isufficient permissions" $
      \(collaborator :: StoredUser)
       (owner :: StoredUser)
       (tid :: TeamId)
       config
       ownDomain
       collabPerms
       ((NonEligibleRole role) :: NonEligibleRole) ->
          let localBackend :: MiniBackend = def {users = [collaborator, owner]}
              authUser = toLocalUnsafe ownDomain owner.id
              perms = rolePermissions role
              ownerTeamMember :: TeamMember = mkTeamMember owner.id perms Nothing UserLegalHoldDisabled
              teamMap = Map.singleton tid [ownerTeamMember]
           in do
                res <-
                  runNoFederationStack
                    localBackend
                    teamMap
                    config
                    $ catchExpectedError @TeamCollaboratorsError
                      (createTeamCollaborator authUser collaborator.id tid collabPerms)
                pure $ res === InsufficientRights

    prop "getting fails if the caller has insufficient permissions" $
      \(collaborator :: StoredUser)
       (owner :: StoredUser)
       (teamMember :: StoredUser)
       (tid :: TeamId)
       config
       ownDomain
       collabPerms
       ((EligibleRole eligibleRole) :: EligibleRole)
       ((NonEligibleRole nonEligibleRole) :: NonEligibleRole) ->
          let localBackend :: MiniBackend = def {users = [collaborator, owner, teamMember]}
              eligibleAuthUser = toLocalUnsafe ownDomain owner.id
              nonEligibleAuthUser = toLocalUnsafe ownDomain teamMember.id
              elPerms = rolePermissions eligibleRole
              nonElPerms = rolePermissions nonEligibleRole
              ownerTeamMember :: TeamMember = mkTeamMember owner.id elPerms Nothing UserLegalHoldDisabled
              otherTeamMember :: TeamMember = mkTeamMember teamMember.id nonElPerms Nothing UserLegalHoldDisabled
              teamMap = Map.singleton tid [ownerTeamMember, otherTeamMember]
           in do
                res <-
                  runNoFederationStack
                    localBackend
                    teamMap
                    config
                    $ do
                      createTeamCollaborator eligibleAuthUser collaborator.id tid collabPerms
                      catchExpectedError @TeamCollaboratorsError $ getAllTeamCollaborators nonEligibleAuthUser tid
                pure $ res === InsufficientRights

    prop "creation fails if team does not exist" $
      \(collaborator :: StoredUser)
       (owner :: StoredUser)
       (tid :: TeamId)
       config
       ownDomain
       collabPerms -> do
          let localBackend :: MiniBackend = def {users = [collaborator, owner]}
              authUser = toLocalUnsafe ownDomain owner.id
              teamMap = mempty
           in do
                res <-
                  runNoFederationStack localBackend teamMap config $
                    catchExpectedError @TeamCollaboratorsError
                      (createTeamCollaborator authUser collaborator.id tid collabPerms)
                pure $ res === InsufficientRights

    prop "getting fails if team does not exist" $
      \(collaborator :: StoredUser)
       (owner :: StoredUser)
       (tid :: TeamId)
       config
       ownDomain -> do
          let localBackend :: MiniBackend = def {users = [collaborator, owner]}
              authUser = toLocalUnsafe ownDomain owner.id
              teamMap = mempty
           in do
                res <-
                  runNoFederationStack localBackend teamMap config $
                    catchExpectedError @TeamCollaboratorsError
                      (getAllTeamCollaborators authUser tid)
                pure $ res === InsufficientRights
  describe "InternalGetTeamCollaborations" $ do
    -- The collaboratorTeams parameter leads to quadratic complextity: Limit
    -- the amount of elements as this test mostly checks our test code anyways.
    modifyMaxSize (const 10) $
      prop "gets all collaborations for all teams for a collaborator" $
        \(owner :: StoredUser)
         (collaboratorTeams :: Map StoredUser [TeamId])
         config
         ownDomain
         collabPerms
         ((EligibleRole role) :: EligibleRole) -> do
            let localBackend :: MiniBackend = def {users = owner : (Map.keys collaboratorTeams)}
                authUser = toLocalUnsafe ownDomain owner.id
                perms = rolePermissions role
                ownerTeamMember :: TeamMember = mkTeamMember owner.id perms Nothing UserLegalHoldDisabled
                teamMap = Map.fromList $ concatMap (\(_, tids) -> map (,[ownerTeamMember]) tids) $ Map.toList collaboratorTeams
             in runNoFederationStack localBackend teamMap config $ do
                  conjoin <$$> forM (Map.keys collaboratorTeams) $ \(collaborator :: StoredUser) -> do
                    forM_ (collaboratorTeams Map.! collaborator) \tid ->
                      createTeamCollaborator authUser collaborator.id tid collabPerms
                    collaborators <- internalGetTeamCollaborations collaborator.id
                    let collaboratorTids = Set.fromList $ map gTeam collaborators
                        expectedCollaboratorTids = collaboratorTeams Map.! collaborator
                    pure $
                      length collaborators === length expectedCollaboratorTids
                        .&&. collaboratorTids === (Set.fromList expectedCollaboratorTids)
  describe "InternalGetAllTeamCollaborators" $ do
    prop "gets all collaborators of a team" $
      \(owner :: StoredUser)
       (team :: TeamId)
       (collaborators :: [StoredUser])
       config
       ownDomain
       collabPerms
       ((EligibleRole role) :: EligibleRole) -> do
          let localBackend :: MiniBackend = def {users = owner : collaborators}
              authUser = toLocalUnsafe ownDomain owner.id
              perms = rolePermissions role
              ownerTeamMember :: TeamMember = mkTeamMember owner.id perms Nothing UserLegalHoldDisabled
              teamMap = Map.singleton team [ownerTeamMember]
           in runNoFederationStack localBackend teamMap config $ do
                forM_ collaborators $ \(collaborator :: StoredUser) ->
                  createTeamCollaborator authUser collaborator.id team collabPerms
                foundCollaborators <- internalGetAllTeamCollaborators team
                let collaboratorIds = Set.fromList $ map gUser foundCollaborators
                    expectedCollaboratorIds = Set.fromList $ map (.id) collaborators
                pure $
                  length collaborators === length foundCollaborators
                    .&&. collaboratorIds === expectedCollaboratorIds

eligibleRoles :: [Role]
eligibleRoles = [RoleAdmin, RoleOwner]

newtype EligibleRole = EligibleRole Role
  deriving (Eq, Show)

instance Arbitrary EligibleRole where
  arbitrary = EligibleRole <$> elements eligibleRoles

newtype NonEligibleRole = NonEligibleRole Role
  deriving (Eq, Show)

instance Arbitrary NonEligibleRole where
  arbitrary = NonEligibleRole <$> elements ([minBound ..] \\ eligibleRoles)
