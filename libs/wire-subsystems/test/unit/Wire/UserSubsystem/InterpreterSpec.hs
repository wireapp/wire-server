{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Wire.UserSubsystem.InterpreterSpec (spec) where

import Data.Bifunctor (first)
import Data.Coerce
import Data.Default (Default (def))
import Data.Domain
import Data.Id
import Data.LegalHold (defUserLegalHoldStatus)
import Data.Qualified
import Data.Set qualified as S
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.State
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Federation.Error
import Wire.API.Team.Feature (AllFeatureConfigs (afcMlsE2EId), FeatureStatus (..), defFeatureStatus, setStatus)
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User hiding (DeleteUser)
import Wire.MiniBackend
import Wire.StoredUser
import Wire.UserStore
import Wire.UserSubsystem
import Wire.UserSubsystem.Interpreter

spec :: Spec
spec = describe "UserSubsystem.Interpreter" do
  describe "getUserProfiles" do
    describe "[with federation]" do
      prop "gets all users on multiple federating backends" $
        \viewerTeam (localTargetUsersNotPending :: [NotPendingStoredUser]) targetUsers1 targetUsers2 visibility localDomain remoteDomain1 remoteDomain2 -> do
          let remoteBackend1 = def {users = targetUsers1}
              remoteBackend2 = def {users = targetUsers2}
              viewer = viewerTeam {teamId = Nothing}
              -- Having teams adds complications in email visibility,
              -- all that stuff is tested in [without federation] tests
              localTargetUsers = map (\user -> (coerce user) {teamId = Nothing}) localTargetUsersNotPending
              federation = [(remoteDomain1, remoteBackend1), (remoteDomain2, remoteBackend2)]
              mkUserIds domain = map (flip Qualified domain . (.id))
              localTargets = mkUserIds localDomain localTargetUsers
              target1 = mkUserIds remoteDomain1 targetUsers1
              target2 = mkUserIds remoteDomain2 targetUsers2
              localBackend = def {users = [viewer] <> localTargetUsers}
              retrievedProfiles =
                runFederationStack localBackend federation Nothing (UserSubsystemConfig visibility miniLocale) $
                  getUserProfiles
                    (toLocalUnsafe localDomain viewer.id)
                    (localTargets <> target1 <> target2)
              mkExpectedProfiles domain users =
                [ mkUserProfileWithEmail
                    Nothing
                    (mkUserFromStored domain miniLocale targetUser)
                    defUserLegalHoldStatus
                  | targetUser <- users
                ]
              expectedLocalProfiles = mkExpectedProfiles localDomain localTargetUsers
              expectedProfiles1 = mkExpectedProfiles remoteDomain1 targetUsers1
              expectedProfiles2 = mkExpectedProfiles remoteDomain2 targetUsers2
              expectedProfiles = expectedLocalProfiles <> expectedProfiles1 <> expectedProfiles2

          sortOn (.profileQualifiedId) retrievedProfiles
            === sortOn (.profileQualifiedId) expectedProfiles

    prop "fails when a backend is offline or returns an error" $
      \viewer onlineTargetUsers (offlineTargetUsers :: [StoredUser]) visibility localDomain onlineDomain (offlineDomain :: Domain) -> do
        let onlineRemoteBackend = def {users = onlineTargetUsers}
            online = [(onlineDomain, onlineRemoteBackend)]
            mkUserIds domain users = map (flip Qualified domain . (.id)) users
            onlineUsers = mkUserIds onlineDomain onlineTargetUsers
            offlineUsers = mkUserIds offlineDomain offlineTargetUsers
            config = UserSubsystemConfig visibility miniLocale
            localBackend = def {users = [viewer]}
            result =
              run
                . runErrorUnsafe @UserSubsystemError
                . runError @FederationError
                . interpretFederationStack localBackend online Nothing config
                $ getUserProfiles
                  (toLocalUnsafe localDomain viewer.id)
                  (onlineUsers <> offlineUsers)

        localDomain /= offlineDomain && not (null offlineTargetUsers) ==>
          -- The FederationError doesn't have an instance
          -- for Eq because of dependency on HTTP2Error
          first displayException result
            === Left (displayException (FederationUnexpectedError "RunFederatedEither"))

    describe "[without federation]" do
      prop "returns nothing when none of the users exist" $
        \viewer targetUserIds visibility domain locale ->
          let config = UserSubsystemConfig visibility locale
              retrievedProfiles =
                runNoFederationStack def Nothing config $
                  getUserProfiles (toLocalUnsafe domain viewer) (map (`Qualified` domain) targetUserIds)
           in retrievedProfiles === []

      prop "gets a local user profile when the user exists and both user and viewer have accepted their invitations" $
        \(NotPendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing defUserLegalHoldStatus
              targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
              config = UserSubsystemConfig visibility locale
              localBackend = def {users = [targetUser, viewer]}
              retrievedProfiles =
                runNoFederationStack localBackend (Just teamMember) config $
                  getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
           in retrievedProfiles
                === [ mkUserProfile
                        (fmap (const $ (,) <$> viewer.teamId <*> Just teamMember) visibility)
                        (mkUserFromStored domain locale targetUser)
                        defUserLegalHoldStatus
                    ]

      prop "gets a local user profile when the target user exists and has accepted their invitation but the viewer has not accepted their invitation" $
        \(PendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing defUserLegalHoldStatus
              targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
              config = UserSubsystemConfig visibility locale
              localBackend = def {users = [targetUser, viewer]}
              retrievedProfile =
                runNoFederationStack localBackend (Just teamMember) config $
                  getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
           in retrievedProfile
                === [ mkUserProfile
                        (fmap (const Nothing) visibility)
                        (mkUserFromStored domain locale targetUser)
                        defUserLegalHoldStatus
                    ]

      prop "returns Nothing if the target user has not accepted their invitation yet" $
        \viewer (PendingStoredUser targetUser) visibility domain locale ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing defUserLegalHoldStatus
              config = UserSubsystemConfig visibility locale
              localBackend = def {users = [targetUser, viewer]}
              retrievedProfile =
                runNoFederationStack localBackend (Just teamMember) config $
                  getLocalUserProfiles (toLocalUnsafe domain [targetUser.id])
           in retrievedProfile === []

  describe "getUserProfilesWithErrors" $ do
    prop "If no errors, same behavior as getUserProfiles" $
      \viewer targetUsers visibility domain remoteDomain -> do
        let remoteBackend = def {users = targetUsers}
            federation = [(remoteDomain, remoteBackend)]
            config = UserSubsystemConfig visibility miniLocale
            localBackend = def {users = [viewer]}
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStack localBackend federation Nothing config $
                getUserProfilesWithErrors
                  (toLocalUnsafe domain viewer.id)
                  (map (flip Qualified remoteDomain . (.id)) targetUsers)
            retrievedProfiles :: [UserProfile] =
              runFederationStack localBackend federation Nothing config $
                getUserProfiles
                  (toLocalUnsafe domain viewer.id)
                  (map (flip Qualified remoteDomain . (.id)) targetUsers)
        remoteDomain /= domain ==>
          counterexample ("Retrieved profiles with errors: " <> show retrievedProfilesWithErrors) do
            length (fst retrievedProfilesWithErrors) === 0
            .&&. snd retrievedProfilesWithErrors === retrievedProfiles
            .&&. length (snd retrievedProfilesWithErrors) === length targetUsers

    prop "Remote users on offline backend always fail to return" $
      \viewer (targetUsers :: Set StoredUser) visibility domain remoteDomain -> do
        let online = mempty
            config = UserSubsystemConfig visibility miniLocale
            localBackend = def {users = [viewer]}
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStack localBackend online Nothing config $
                getUserProfilesWithErrors
                  (toLocalUnsafe domain viewer.id)
                  ( map (flip Qualified remoteDomain . (.id)) $
                      S.toList targetUsers
                  )
        remoteDomain /= domain ==>
          length (fst retrievedProfilesWithErrors) === length targetUsers
            .&&. length (snd retrievedProfilesWithErrors) === 0

    prop "Remote users with one offline and one online backend return errors for offline backend but successed with online backend" $
      \viewer targetUsers visibility domain remoteDomainA remoteDomainB -> do
        let remoteBackendA = def {users = targetUsers}
            online = [(remoteDomainA, remoteBackendA)]
            allDomains = [domain, remoteDomainA, remoteDomainB]
            remoteAUsers = map (flip Qualified remoteDomainA . (.id)) targetUsers
            remoteBUsers = map (flip Qualified remoteDomainB . (.id)) targetUsers
            config = UserSubsystemConfig visibility miniLocale
            localBackend = def {users = [viewer]}
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStack localBackend online Nothing config $
                getUserProfilesWithErrors
                  (toLocalUnsafe domain viewer.id)
                  (remoteAUsers <> remoteBUsers)
        nub allDomains == allDomains ==>
          length (fst retrievedProfilesWithErrors) === length remoteBUsers
            .&&. length (snd retrievedProfilesWithErrors) === length remoteAUsers

    prop "Update user" $
      \(NotPendingStoredUser alice) localDomain update config -> do
        let lusr = toLocalUnsafe localDomain alice.id
            localBackend = def {users = [alice {managedBy = Just ManagedByWire}]}
            profile = fromJust $ runNoFederationStack localBackend Nothing config do
              updateUserProfile
                lusr
                Nothing
                update
              getUserProfile lusr (tUntagged lusr)
         in -- TODO: check locale update?
            profile.profileQualifiedId === tUntagged lusr
              -- if the name / pict / etc... is not set, the original
              -- value should be preserved
              .&&. profile.profileName === maybe profile.profileName (.value) update.name
              .&&. profile.profilePict === fromMaybe profile.profilePict update.pict
              .&&. profile.profileAssets === fromMaybe profile.profileAssets update.assets
              .&&. profile.profileAccentId === fromMaybe profile.profileAccentId update.accentId

    prop "Update user events" $
      \(NotPendingStoredUser alice) localDomain update config -> do
        let lusr = toLocalUnsafe localDomain alice.id
            localBackend = def {users = [alice {managedBy = Just ManagedByWire}]}
            events = runNoFederationStack localBackend Nothing config do
              updateUserProfile lusr Nothing update
              get @[MiniEvent]
         in events === [MkMiniEvent alice.id (mkProfileUpdateEvent alice.id update)]

    prop
      "user managed by scim doesn't allow update"
      \(NotPendingStoredUser alice) localDomain (update :: UserProfileUpdate) name config ->
        alice.name /= name ==>
          let lusr = toLocalUnsafe localDomain alice.id
              localBackend = def {users = [alice {managedBy = Just ManagedByScim}]}
              profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                run
                  . runErrorUnsafe
                  . runError
                  $ interpretNoFederationStack localBackend Nothing def config do
                    updateUserProfile lusr Nothing update {name = Just (forbidScimUpdate name)}
                    getUserProfile lusr (tUntagged lusr)
           in Left UserSubsystemDisplayNameManagedByScim === profileErr

    prop
      "if e2e identity is activated, the user name cannot be updated"
      \(NotPendingStoredUser alice) localDomain update name config ->
        alice.name /= name ==>
          let lusr = toLocalUnsafe localDomain alice.id
              localBackend = def {users = [alice]}
              profileErr :: Either UserSubsystemError (Maybe UserProfile) =
                run
                  . runErrorUnsafe
                  . runError
                  $ interpretNoFederationStack localBackend Nothing def {afcMlsE2EId = setStatus FeatureStatusEnabled defFeatureStatus} config do
                    updateUserProfile lusr Nothing update {name = Just (allowScimUpdate name)}
                    getUserProfile lusr (tUntagged lusr)
           in Left UserSubsystemDisplayNameManagedByScim === profileErr
