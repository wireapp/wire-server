{-# LANGUAGE OverloadedLists #-}

module Wire.UserSubsystem.InterpreterSpec (spec) where

import Data.Bifunctor (first)
import Data.Coerce
import Data.Default (Default (def))
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldDisabled))
import Data.Qualified
import Data.Set qualified as S
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Federation.Error
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.User hiding (DeleteUser)
import Wire.MiniBackend
import Wire.StoredUser
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
              localTargetUsers =
                S.fromList $
                  map (\user -> (coerce user) {teamId = Nothing}) localTargetUsersNotPending
              federation = [(remoteDomain1, remoteBackend1), (remoteDomain2, remoteBackend2)]
              mkUserIds domain = map (flip Qualified domain . (.id)) . S.toList
              localTargets = mkUserIds localDomain localTargetUsers
              target1 = mkUserIds remoteDomain1 targetUsers1
              target2 = mkUserIds remoteDomain2 targetUsers2
              retrievedProfiles =
                runFederationStack ([viewer] <> S.toList localTargetUsers) federation Nothing (UserSubsystemConfig visibility miniLocale) $
                  getUserProfiles
                    (toLocalUnsafe localDomain viewer.id)
                    (localTargets <> target1 <> target2)
              mkExpectedProfiles domain users =
                [ mkUserProfileWithEmail
                    Nothing
                    (mkUserFromStored domain miniLocale targetUser)
                    UserLegalHoldDisabled
                  | targetUser <- S.toList users
                ]
              expectedLocalProfiles = mkExpectedProfiles localDomain localTargetUsers
              expectedProfiles1 = mkExpectedProfiles remoteDomain1 targetUsers1
              expectedProfiles2 = mkExpectedProfiles remoteDomain2 targetUsers2
              expectedProfiles = expectedLocalProfiles <> expectedProfiles1 <> expectedProfiles2

          sortOn (.profileQualifiedId) retrievedProfiles
            === sortOn (.profileQualifiedId) expectedProfiles

    prop "fails when a backend is offline or returns an error" $
      \viewer onlineTargetUsers offlineTargetUsers visibility localDomain onlineDomain offlineDomain -> do
        let onlineRemoteBackend = def {users = onlineTargetUsers}
            offlineRemoteBackend = def {users = offlineTargetUsers}
            online = [(onlineDomain, onlineRemoteBackend)]
            offline = [(offlineDomain, offlineRemoteBackend)]
            mkUserIds domain users = map (flip Qualified domain . (.id)) (S.toList users)
            onlineUsers = mkUserIds onlineDomain onlineTargetUsers
            offlineUsers = mkUserIds offlineDomain offlineTargetUsers
            config = UserSubsystemConfig visibility miniLocale

            result =
              runFederationStackWithUnavailableBackendsEither [viewer] online offline Nothing config $
                getUserProfiles
                  (toLocalUnsafe localDomain viewer.id)
                  (onlineUsers <> offlineUsers)
        localDomain /= offlineDomain && offlineTargetUsers /= [] ==>
          -- The FederationError doesn't have an instance
          -- for Eq because of dependency on HTTP2Error
          first (displayException) result
            === Left (displayException (FederationUnexpectedError "RunFederatedEither"))

    describe "[without federation]" do
      prop "returns nothing when none of the users exist" $
        \viewer targetUserIds visibility domain locale ->
          let config = UserSubsystemConfig visibility locale
              retrievedProfiles =
                runNoFederationStack [] Nothing config $
                  getUserProfiles (toLocalUnsafe domain viewer) (map (`Qualified` domain) targetUserIds)
           in retrievedProfiles === []

      prop "gets a local user profile when the user exists and both user and viewer have accepted their invitations" $
        \(NotPendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing UserLegalHoldDisabled
              targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
              config = UserSubsystemConfig visibility locale
              retrievedProfiles =
                runNoFederationStack [targetUser, viewer] (Just teamMember) config $
                  getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
           in retrievedProfiles
                === [ mkUserProfile
                        (fmap (const $ (,) <$> viewer.teamId <*> Just teamMember) visibility)
                        (mkUserFromStored domain locale targetUser)
                        UserLegalHoldDisabled
                    ]

      prop "gets a local user profile when the target user exists and has accepted their invitation but the viewer has not accepted their invitation" $
        \(PendingStoredUser viewer) (NotPendingStoredUser targetUserNoTeam) visibility domain locale sameTeam ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing UserLegalHoldDisabled
              targetUser = if sameTeam then targetUserNoTeam {teamId = viewer.teamId} else targetUserNoTeam
              config = UserSubsystemConfig visibility locale
              retrievedProfile =
                runNoFederationStack [targetUser, viewer] (Just teamMember) config $
                  getUserProfiles (toLocalUnsafe domain viewer.id) [Qualified targetUser.id domain]
           in retrievedProfile
                === [ mkUserProfile
                        (fmap (const Nothing) visibility)
                        (mkUserFromStored domain locale targetUser)
                        UserLegalHoldDisabled
                    ]

      prop "returns Nothing if the target user has not accepted their invitation yet" $
        \viewer (PendingStoredUser targetUser) visibility domain locale ->
          let teamMember = mkTeamMember viewer.id fullPermissions Nothing UserLegalHoldDisabled
              config = UserSubsystemConfig visibility locale
              retrievedProfile =
                runNoFederationStack [targetUser, viewer] (Just teamMember) config $
                  getLocalUserProfiles (toLocalUnsafe domain [targetUser.id])
           in retrievedProfile === []

  describe "getUserProfilesWithErrors" $ do
    prop "If no errors, same behavior as getUserProfiles" $
      \viewer targetUsers visibility domain remoteDomain -> do
        let remoteBackend = def {users = targetUsers}
            federation = [(remoteDomain, remoteBackend)]
            config = UserSubsystemConfig visibility miniLocale
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStackWithUnavailableBackends [viewer] federation mempty Nothing config $
                getUserProfilesWithErrors
                  (toLocalUnsafe domain viewer.id)
                  ( map (flip Qualified remoteDomain . (.id)) $
                      S.toList targetUsers
                  )
            retrievedProfiles :: [UserProfile] =
              runFederationStack [viewer] federation Nothing config $
                getUserProfiles
                  (toLocalUnsafe domain viewer.id)
                  ( map (flip Qualified remoteDomain . (.id)) $
                      S.toList targetUsers
                  )
        remoteDomain /= domain ==>
          counterexample ("Retrieved profiles with errors: " <> show retrievedProfilesWithErrors) do
            length (fst retrievedProfilesWithErrors) === 0
            .&&. snd retrievedProfilesWithErrors === retrievedProfiles
            .&&. length (snd retrievedProfilesWithErrors) === length targetUsers

    prop "Remote users on offline backend always fail to return" $
      \viewer targetUsers visibility domain remoteDomain -> do
        let remoteBackend = def {users = targetUsers}
            offline = [(remoteDomain, remoteBackend)]
            online = mempty
            config = UserSubsystemConfig visibility miniLocale
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStackWithUnavailableBackends [viewer] online offline Nothing config $
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
            remoteBackendB = def {users = targetUsers}
            online = [(remoteDomainA, remoteBackendA)]
            offline = [(remoteDomainB, remoteBackendB)]
            allDomains = [domain, remoteDomainA, remoteDomainB]
            remoteAUsers = map (flip Qualified remoteDomainA . (.id)) (S.toList targetUsers)
            remoteBUsers = map (flip Qualified remoteDomainB . (.id)) (S.toList targetUsers)
            config = UserSubsystemConfig visibility miniLocale
            retrievedProfilesWithErrors :: ([(Qualified UserId, FederationError)], [UserProfile]) =
              runFederationStackWithUnavailableBackends [viewer] online offline Nothing config $
                getUserProfilesWithErrors
                  (toLocalUnsafe domain viewer.id)
                  (remoteAUsers <> remoteBUsers)
        nub allDomains == allDomains ==>
          length (fst retrievedProfilesWithErrors) === length remoteBUsers
            .&&. length (snd retrievedProfilesWithErrors) === length remoteAUsers
